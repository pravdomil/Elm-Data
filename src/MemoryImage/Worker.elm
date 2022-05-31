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
    , status : Status

    --
    , overwriteQueue : Maybe MemoryImage.OverwriteData
    , appendQueue : List MemoryImage.AppendData
    }


emptyInitializedModel : MemoryImage.MemoryImage msg a -> FileSystem.Handle.Handle -> InitializedModel msg a
emptyInitializedModel image handle =
    InitializedModel image handle Idle Nothing []



--


type Status
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
                            in
                            ( Just (emptyInitializedModel image handle)
                            , sendMessage (config.gotFlags b |> GotMessage)
                            )

                        Nothing ->
                            let
                                ( image, cmd, data ) =
                                    MemoryImage.create config.image config.init

                                nextModel : InitializedModel msg a
                                nextModel =
                                    emptyInitializedModel image handle
                            in
                            ( Just { nextModel | overwriteQueue = Just data, appendQueue = [] }
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
                    in
                    ( Just { c | image = image, appendQueue = data :: c.appendQueue }
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
                    case b.status of
                        Idle ->
                            let
                                suffix : String
                                suffix =
                                    b.appendQueue
                                        |> List.reverse
                                        |> List.map (\(MemoryImage.AppendData v) -> v)
                                        |> String.join ""
                            in
                            case b.overwriteQueue of
                                Just (MemoryImage.OverwriteData c) ->
                                    ( Just { b | status = Busy, overwriteQueue = Nothing, appendQueue = [] }
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
                                        ( Just { b | status = Busy, overwriteQueue = Nothing, appendQueue = [] }
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
                            ( Just { c | status = Idle }
                            , sendMessage DoQueue
                            )

                        Err d ->
                            let
                                data : MemoryImage.OverwriteData
                                data =
                                    MemoryImage.save config.image c.image
                            in
                            ( Just { c | overwriteQueue = Just data, appendQueue = [] }
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
                    let
                        data : MemoryImage.OverwriteData
                        data =
                            MemoryImage.save config.image b.image
                    in
                    ( Just { b | overwriteQueue = Just data, appendQueue = [] }
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
