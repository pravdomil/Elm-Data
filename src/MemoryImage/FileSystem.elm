module MemoryImage.FileSystem exposing (Image, Msg, close, image, init, subscriptions, update, updateMsg)

import Console
import FileSystem
import FileSystem.Handle
import JavaScript
import Json.Encode
import MemoryImage
import Process
import Process.Extra
import Task
import Time


type Image msg a
    = Image (Model msg a)


image : Image msg a -> Maybe a
image (Image a) =
    a.image |> Maybe.map (Tuple.second >> MemoryImage.image)



--


type alias Model msg a =
    { image : Maybe ( FileSystem.Handle.Handle, MemoryImage.MemoryImage msg a )
    , status : Status
    , queue : Queue msg a
    , queueStatus : QueueStatus
    }



--


init : FileSystem.Path -> ( Image msg a, Cmd (Msg msg) )
init path =
    ( Image (Model Nothing Running Empty Idle)
    , getHandle path
        |> Task.attempt GotHandle
    )


update : msg -> Cmd (Msg msg)
update a =
    sendMessage (GotMessage a)


close : Cmd (Msg msg)
close =
    sendMessage PleaseClose


subscriptions : Image msg a -> Sub (Msg msg)
subscriptions (Image a) =
    case a.status of
        Running ->
            Time.every (1000 * 60 * 60 * 24) (\_ -> DayElapsed)

        Exiting ->
            Sub.none



--


type Msg msg
    = GotHandle (Result JavaScript.Error ( FileSystem.Handle.Handle, String ))
    | GotMessage msg
    | DoQueue
    | QueueDone (Result JavaScript.Error ())
    | DayElapsed
    | PleaseClose
    | NoOperation


updateMsg : MemoryImage.Config msg a -> (() -> ( a, Cmd msg )) -> (msg -> a -> ( a, Cmd msg )) -> Msg msg -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
updateMsg config initFn updateFn msg (Image a) =
    case msg of
        GotHandle b ->
            let
                toDiskImage : ( FileSystem.Handle.Handle, String ) -> Result JavaScript.Error ( FileSystem.Handle.Handle, Maybe (MemoryImage.DiskImage msg a) )
                toDiskImage ( handle, c ) =
                    case c of
                        "" ->
                            Ok ( handle, Nothing )

                        _ ->
                            MemoryImage.decodeDiskImage config c
                                |> Result.mapError JavaScript.DecodeError
                                |> Result.map (\v -> ( handle, Just v ))
            in
            case b |> Result.andThen toDiskImage of
                Ok ( handle, c ) ->
                    case c of
                        Just d ->
                            ( Image { a | image = Just ( handle, MemoryImage.diskImageToMemoryImage updateFn d ) }
                            , Cmd.none
                            )

                        Nothing ->
                            let
                                ( image_, cmd ) =
                                    MemoryImage.init initFn
                            in
                            ( Image { a | image = Just ( handle, image_ ), queue = SaveImage }
                            , cmd |> Cmd.map GotMessage
                            )

                Err c ->
                    ( Image a
                    , Cmd.batch
                        [ Process.Extra.exit 1
                            |> Task.attempt (\_ -> NoOperation)
                        , Console.logError ("Cannot load memory image. See details:\n" ++ JavaScript.errorToString c)
                            |> Task.attempt (\_ -> NoOperation)
                        ]
                    )

        GotMessage b ->
            case a.image of
                Just ( handle, c ) ->
                    let
                        ( image_, cmd ) =
                            MemoryImage.update updateFn b c
                    in
                    ( Image { a | image = Just ( handle, image_ ), queue = queueAddLogMessage b a.queue }
                    , Cmd.batch
                        [ cmd |> Cmd.map GotMessage
                        , sendMessage DoQueue
                        ]
                    )

                Nothing ->
                    ( Image a
                    , Cmd.none
                    )

        DoQueue ->
            case a.image of
                Just ( handle, image_ ) ->
                    case a.queueStatus of
                        Idle ->
                            case a.queue of
                                Empty ->
                                    ( Image a
                                    , Cmd.none
                                    )

                                LogMessage first rest ->
                                    let
                                        data : String
                                        data =
                                            encodeMessages config (first :: rest)
                                    in
                                    ( Image { a | queue = Empty, queueStatus = Busy }
                                    , FileSystem.Handle.write data handle
                                        |> Task.map (\_ -> ())
                                        |> Task.attempt QueueDone
                                    )

                                SaveImage ->
                                    let
                                        data : String
                                        data =
                                            Json.Encode.encode 0 (config.encoder (MemoryImage.image image_))
                                    in
                                    ( Image { a | queue = Empty, queueStatus = Busy }
                                    , FileSystem.Handle.truncate handle
                                        |> Task.andThen (FileSystem.Handle.write data)
                                        |> Task.map (\_ -> ())
                                        |> Task.attempt QueueDone
                                    )

                        Busy ->
                            ( Image a
                            , Cmd.none
                            )

                Nothing ->
                    ( Image a
                    , Cmd.none
                    )

        QueueDone b ->
            case b of
                Ok _ ->
                    ( Image { a | queueStatus = Idle }
                    , sendMessage DoQueue
                    )

                Err d ->
                    ( Image { a | queue = SaveImage, queueStatus = Idle }
                    , Cmd.batch
                        [ Console.logError ("Cannot save memory image. See details:\n" ++ JavaScript.errorToString d)
                            |> Task.attempt (\_ -> NoOperation)
                        , sendMessageAfter 1000 DoQueue
                        ]
                    )

        DayElapsed ->
            ( Image { a | queue = SaveImage }
            , sendMessage DoQueue
            )

        PleaseClose ->
            ( Image { a | status = Exiting, queue = SaveImage }
            , sendMessage DoQueue
            )

        NoOperation ->
            ( Image a
            , Cmd.none
            )



--


getHandle : FileSystem.Path -> Task.Task JavaScript.Error ( FileSystem.Handle.Handle, String )
getHandle path =
    let
        mode : FileSystem.Handle.Mode
        mode =
            FileSystem.Handle.Mode
                FileSystem.Handle.ReadAndWrite
                FileSystem.Handle.CreateIfNotExists
                FileSystem.Handle.DoNotTruncate
                FileSystem.Handle.Append
    in
    FileSystem.Handle.open mode path
        |> Task.andThen
            (\v ->
                FileSystem.Handle.read v
                    |> Task.map (\v2 -> ( v, v2 ))
            )



--


type Status
    = Running
    | Exiting



--


type Queue msg a
    = Empty
    | LogMessage msg (List msg)
    | SaveImage


queueAddLogMessage : msg -> Queue msg a -> Queue msg a
queueAddLogMessage data a =
    case a of
        Empty ->
            LogMessage data []

        SaveImage ->
            SaveImage

        LogMessage first rest ->
            LogMessage data (first :: rest)


encodeMessages : MemoryImage.Config msg a -> List msg -> String
encodeMessages config a =
    a
        |> List.reverse
        |> List.map (\v -> v |> config.msgEncoder |> Json.Encode.encode 0 |> (++) "\n")
        |> String.join ""



--


type QueueStatus
    = Idle
    | Busy



--


sendMessage : a -> Cmd a
sendMessage a =
    Task.succeed () |> Task.perform (\() -> a)


sendMessageAfter : Float -> msg -> Cmd msg
sendMessageAfter a msg =
    Process.sleep a |> Task.perform (\() -> msg)
