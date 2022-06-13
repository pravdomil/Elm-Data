module MemoryImage.FileSystem exposing (Model, Msg, close, open, subscriptions, update, updateMsg)

import Console
import FileSystem
import FileSystem.Handle
import JavaScript
import Json.Encode
import MemoryImage
import Process
import Task
import Time


type Model msg a
    = Model
        { image : MemoryImage.MemoryImage msg a
        , status : Status
        , handle : FileSystem.Handle.Handle
        , queue : Queue msg a
        , queueStatus : QueueStatus
        }


emptyModel : MemoryImage.MemoryImage msg a -> FileSystem.Handle.Handle -> Queue msg a -> Model msg a
emptyModel a b c =
    Model { image = a, status = Running, handle = b, queue = c, queueStatus = Idle }



--


open : MemoryImage.Config msg a -> (() -> a) -> (msg -> a -> a) -> FileSystem.Path -> Task.Task JavaScript.Error (Model msg a)
open config initFn updateFn path =
    getDiskImage config path
        |> Task.map
            (\( handle, b ) ->
                case b of
                    Just c ->
                        let
                            image : MemoryImage.MemoryImage msg a
                            image =
                                MemoryImage.diskImageToMemoryImage updateFn c
                        in
                        emptyModel image handle Empty

                    Nothing ->
                        let
                            ( image, data ) =
                                MemoryImage.create initFn
                        in
                        emptyModel image handle (queueAddSaveImage data)
            )


update : (msg -> a -> a) -> msg -> Model msg a -> ( Model msg a, Cmd Msg )
update updateFn msg (Model a) =
    let
        ( image, logMessage ) =
            MemoryImage.update updateFn msg a.image

        queue : Queue msg a
        queue =
            case a.status of
                Running ->
                    queueAddLogMessage logMessage a.queue

                Exiting ->
                    queueAddSaveImage (MemoryImage.save a.image)
    in
    ( Model { a | image = image, queue = queue }
    , sendMessage DoQueue
    )


close : Model msg a -> ( Model msg a, Cmd Msg )
close (Model a) =
    ( Model { a | status = Exiting, queue = queueAddSaveImage (MemoryImage.save a.image) }
    , sendMessage DoQueue
    )


subscriptions : Model msg a -> Sub Msg
subscriptions (Model a) =
    case a.status of
        Running ->
            Time.every (1000 * 60 * 60 * 24) (\_ -> DayElapsed)

        Exiting ->
            Sub.none



--


type Msg
    = DoQueue
    | QueueDone (Result JavaScript.Error ())
    | DayElapsed
    | NoOperation


updateMsg : MemoryImage.Config msg a -> Msg -> Model msg a -> ( Model msg a, Cmd Msg )
updateMsg config msg (Model a) =
    case msg of
        DoQueue ->
            case a.queueStatus of
                Idle ->
                    ( Model { a | queue = Empty }
                    , case a.queue of
                        Empty ->
                            Cmd.none

                        Append _ _ ->
                            FileSystem.Handle.write (queueToString config a.queue) a.handle
                                |> Task.map (\_ -> ())
                                |> Task.attempt QueueDone

                        Overwrite _ _ ->
                            FileSystem.Handle.truncate a.handle
                                |> Task.andThen (FileSystem.Handle.write (queueToString config a.queue))
                                |> Task.map (\_ -> ())
                                |> Task.attempt QueueDone
                    )

                Busy ->
                    ( Model a
                    , Cmd.none
                    )

        QueueDone b ->
            case b of
                Ok _ ->
                    ( Model { a | queueStatus = Idle }
                    , sendMessage DoQueue
                    )

                Err d ->
                    ( Model { a | queue = queueAddSaveImage (MemoryImage.save a.image), queueStatus = Idle }
                    , Cmd.batch
                        [ Console.logError ("Cannot save memory image. See details:\n" ++ JavaScript.errorToString d)
                            |> Task.attempt (\_ -> NoOperation)
                        , sendMessageAfter 1000 DoQueue
                        ]
                    )

        DayElapsed ->
            ( Model { a | queue = queueAddSaveImage (MemoryImage.save a.image) }
            , sendMessage DoQueue
            )

        NoOperation ->
            ( Model a
            , Cmd.none
            )



--


type Status
    = Running
    | Exiting



--


type Queue msg a
    = Empty
    | Append (MemoryImage.LogMessage msg) (List (MemoryImage.LogMessage msg))
    | Overwrite (MemoryImage.SaveImage a) (List (MemoryImage.LogMessage msg))


queueAddLogMessage : MemoryImage.LogMessage msg -> Queue msg a -> Queue msg a
queueAddLogMessage data a =
    case a of
        Empty ->
            Append data []

        Overwrite b c ->
            Overwrite b (data :: c)

        Append first rest ->
            Append data (first :: rest)


queueAddSaveImage : MemoryImage.SaveImage a -> Queue msg a
queueAddSaveImage a =
    Overwrite a []


queueToString : MemoryImage.Config msg a -> Queue msg a -> String
queueToString config a =
    let
        appendDataToString : List (MemoryImage.LogMessage msg) -> String
        appendDataToString c =
            c
                |> List.reverse
                |> List.map
                    (\v ->
                        v
                            |> (\(MemoryImage.LogMessage v2) -> v2)
                            |> config.msgEncoder
                            |> Json.Encode.encode 0
                            |> (++) "\n"
                    )
                |> String.join ""
    in
    case a of
        Empty ->
            ""

        Overwrite b c ->
            (\(MemoryImage.SaveImage v) -> Json.Encode.encode 0 (config.encoder v)) b ++ appendDataToString c

        Append first rest ->
            appendDataToString (first :: rest)



--


type QueueStatus
    = Idle
    | Busy



--


getDiskImage : MemoryImage.Config msg a -> FileSystem.Path -> Task.Task JavaScript.Error ( FileSystem.Handle.Handle, Maybe (MemoryImage.DiskImage msg a) )
getDiskImage config a =
    let
        mode : FileSystem.Handle.Mode
        mode =
            FileSystem.Handle.Mode
                FileSystem.Handle.ReadAndWrite
                FileSystem.Handle.CreateIfNotExists
                FileSystem.Handle.DoNotTruncate
                FileSystem.Handle.Append
    in
    FileSystem.Handle.open mode a
        |> Task.andThen
            (\v ->
                FileSystem.Handle.read v
                    |> Task.andThen
                        (\v2 ->
                            case v2 of
                                "" ->
                                    Task.succeed Nothing

                                _ ->
                                    MemoryImage.decodeDiskImage config v2
                                        |> resultToTask
                                        |> Task.mapError JavaScript.DecodeError
                                        |> Task.map Just
                        )
                    |> Task.map
                        (\v2 ->
                            ( v, v2 )
                        )
            )



--


sendMessage : a -> Cmd a
sendMessage a =
    Task.succeed () |> Task.perform (\() -> a)


sendMessageAfter : Float -> msg -> Cmd msg
sendMessageAfter a msg =
    Process.sleep a |> Task.perform (\() -> msg)


resultToTask : Result x a -> Task.Task x a
resultToTask a =
    case a of
        Ok b ->
            Task.succeed b

        Err b ->
            Task.fail b
