module MemoryImage.Worker exposing (Config, Error(..), worker)

import Console
import FileSystem
import FileSystem.Handle
import JavaScript
import Json.Decode
import MemoryImage
import Process.Extra
import Task
import Time


type alias Config flags msg a =
    { image : MemoryImage.Config msg a
    , imagePath : FileSystem.Path

    --
    , init : () -> ( a, Cmd msg )
    , update : msg -> a -> ( a, Cmd msg )
    , subscriptions : a -> Sub msg

    --
    , gotFlags : flags -> msg
    }



--


type Error
    = DecodeError Json.Decode.Error
    | FileSystemError JavaScript.Error


errorToString : Error -> String
errorToString a =
    case a of
        DecodeError b ->
            Json.Decode.errorToString b

        FileSystemError b ->
            JavaScript.errorToString b



--


worker : Config flags msg a -> Program flags (Model msg a) (Msg flags msg a)
worker config =
    Platform.worker
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
        }



--


type alias Model msg a =
    Maybe (InitializedModel msg a)



--


type alias InitializedModel msg a =
    { image : MemoryImage.MemoryImage msg a
    , handle : FileSystem.Handle.Handle
    , queue : Queue
    }


emptyInitializedModel : MemoryImage.MemoryImage msg a -> FileSystem.Handle.Handle -> InitializedModel msg a
emptyInitializedModel image handle =
    InitializedModel image handle (emptyQueue Idle)



--


type alias Queue =
    { status : QueueStatus
    , overwrite : Maybe MemoryImage.OverwriteData
    , append : List MemoryImage.AppendData
    }


emptyQueue : QueueStatus -> Queue
emptyQueue status =
    Queue status Nothing []


queueChangeStatus : QueueStatus -> Queue -> Queue
queueChangeStatus status a =
    { a | status = status }


queueOverwriteData : MemoryImage.OverwriteData -> Queue -> Queue
queueOverwriteData data a =
    { a | overwrite = Just data, append = [] }


queueAppendData : MemoryImage.AppendData -> Queue -> Queue
queueAppendData data a =
    { a | append = data :: a.append }



--


type QueueStatus
    = Idle
    | Busy



--


init : Config flags msg a -> flags -> ( Model msg a, Cmd (Msg flags msg a) )
init config flags =
    ( Nothing
    , getDiskImage config |> Task.attempt (GotDiskImage flags)
    )


getDiskImage : Config flags msg a -> Task.Task Error ( FileSystem.Handle.Handle, Maybe (MemoryImage.DiskImage msg a) )
getDiskImage config =
    let
        mode : FileSystem.Handle.Mode
        mode =
            FileSystem.Handle.Mode
                FileSystem.Handle.ReadAndWrite
                FileSystem.Handle.CreateIfNotExists
                FileSystem.Handle.DoNotTruncate
                FileSystem.Handle.Append
    in
    FileSystem.Handle.open mode config.imagePath
        |> Task.mapError FileSystemError
        |> Task.andThen
            (\v ->
                FileSystem.Handle.read v
                    |> Task.mapError FileSystemError
                    |> Task.andThen
                        (\v2 ->
                            case v2 of
                                "" ->
                                    Task.succeed Nothing

                                _ ->
                                    MemoryImage.decodeDiskImage config.image v2
                                        |> resultToTask
                                        |> Task.mapError DecodeError
                                        |> Task.map Just
                        )
                    |> Task.map
                        (\v2 ->
                            ( v, v2 )
                        )
            )



--


type Msg flags msg a
    = GotDiskImage flags (Result Error ( FileSystem.Handle.Handle, Maybe (MemoryImage.DiskImage msg a) ))
    | GotMessage msg
    | DoQueue
    | QueueDone (Result Error ())
    | SaveImage
    | NoOperation


update : Config flags msg a -> Msg flags msg a -> Model msg a -> ( Model msg a, Cmd (Msg flags msg a) )
update config msg model =
    case msg of
        GotDiskImage b c ->
            case c of
                Ok ( handle, maybe ) ->
                    case maybe of
                        Just d ->
                            let
                                image : MemoryImage.MemoryImage msg a
                                image =
                                    MemoryImage.diskImageToMemoryImage config.update d

                                nextModel : InitializedModel msg a
                                nextModel =
                                    emptyInitializedModel image handle
                            in
                            ( Just nextModel
                            , sendMessage (config.gotFlags b |> GotMessage)
                            )

                        Nothing ->
                            let
                                ( image, cmd, data ) =
                                    MemoryImage.create config.image config.init

                                nextModel : InitializedModel msg a
                                nextModel =
                                    emptyInitializedModel image handle
                                        |> (\v -> { v | queue = queueOverwriteData data v.queue })
                            in
                            ( Just nextModel
                            , Cmd.batch
                                [ cmd |> Cmd.map GotMessage
                                , sendMessage (config.gotFlags b |> GotMessage)
                                ]
                            )

                Err d ->
                    ( model
                    , Cmd.batch
                        [ Process.Extra.exit 1
                            |> Task.attempt (\_ -> NoOperation)
                        , Console.logError ("Cannot load memory image. See details:\n" ++ errorToString d)
                            |> Task.attempt (\_ -> NoOperation)
                        ]
                    )

        GotMessage b ->
            case model of
                Just c ->
                    let
                        ( image, cmd, data ) =
                            MemoryImage.update config.image config.update b c.image

                        nextModel : InitializedModel msg a
                        nextModel =
                            { c
                                | image = image
                                , queue = queueAppendData data c.queue
                            }
                    in
                    ( Just nextModel
                    , Cmd.batch
                        [ cmd |> Cmd.map GotMessage
                        , sendMessage DoQueue
                        ]
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        DoQueue ->
            case model of
                Just b ->
                    case b.queue.status of
                        Idle ->
                            let
                                suffix : String
                                suffix =
                                    b.queue.append
                                        |> List.reverse
                                        |> List.map (\(MemoryImage.AppendData v) -> v)
                                        |> String.join ""
                            in
                            case b.queue.overwrite of
                                Just (MemoryImage.OverwriteData c) ->
                                    ( Just { b | queue = emptyQueue Busy }
                                    , FileSystem.Handle.truncate b.handle
                                        |> Task.andThen (FileSystem.Handle.write (c ++ suffix))
                                        |> Task.map (\_ -> ())
                                        |> Task.mapError FileSystemError
                                        |> Task.attempt QueueDone
                                    )

                                Nothing ->
                                    if suffix == "" then
                                        ( model
                                        , Cmd.none
                                        )

                                    else
                                        ( Just { b | queue = emptyQueue Busy }
                                        , FileSystem.Handle.write suffix b.handle
                                            |> Task.map (\_ -> ())
                                            |> Task.mapError FileSystemError
                                            |> Task.attempt QueueDone
                                        )

                        Busy ->
                            ( model
                            , Cmd.none
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        QueueDone b ->
            case model of
                Just c ->
                    case b of
                        Ok _ ->
                            let
                                nextModel : InitializedModel msg a
                                nextModel =
                                    { c | queue = queueChangeStatus Idle c.queue }
                            in
                            ( Just nextModel
                            , sendMessage DoQueue
                            )

                        Err d ->
                            let
                                nextModel : InitializedModel msg a
                                nextModel =
                                    { c
                                        | queue =
                                            c.queue
                                                |> queueChangeStatus Idle
                                                |> queueOverwriteData (MemoryImage.save config.image c.image)
                                    }
                            in
                            ( Just nextModel
                            , Console.logError ("Cannot save memory image. See details:\n" ++ errorToString d)
                                |> Task.attempt (\_ -> NoOperation)
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SaveImage ->
            case model of
                Just b ->
                    ( Just { b | queue = queueOverwriteData (MemoryImage.save config.image b.image) b.queue }
                    , sendMessage DoQueue
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        NoOperation ->
            ( model
            , Cmd.none
            )



--


subscriptions : Config flags msg a -> Model msg a -> Sub (Msg flags msg a)
subscriptions config model =
    case model of
        Just b ->
            Sub.batch
                [ Time.every (1000 * 60 * 60 * 24) (\_ -> SaveImage)
                , config.subscriptions (MemoryImage.image b.image) |> Sub.map GotMessage
                ]

        Nothing ->
            Sub.none



--


sendMessage : a -> Cmd a
sendMessage a =
    Task.succeed () |> Task.perform (\() -> a)


resultToTask : Result x a -> Task.Task x a
resultToTask a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b
