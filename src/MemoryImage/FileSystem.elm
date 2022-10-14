module MemoryImage.FileSystem exposing
    ( Image, image
    , Msg, init, update, subscriptions
    , close, sendMessage
    )

{-|

@docs Image, image

@docs Msg, init, update, subscriptions

@docs close, sendMessage

-}

import Console
import FileSystem
import FileSystem.Handle
import JavaScript
import Json.Encode
import MemoryImage.FileImage
import Process
import Process.Extra
import Task
import Time


type Image msg a
    = Image (Model msg a)


image : Image msg a -> Maybe a
image (Image a) =
    case a.image of
        NoImage ->
            Nothing

        LoadingImage _ _ ->
            Nothing

        ReadyImage _ _ b ->
            Just (MemoryImage.FileImage.image b)


sendMessage : msg -> Cmd (Msg msg)
sendMessage a =
    sendMessageToSelf (GotMessage a)


close : Cmd (Msg msg)
close =
    sendMessageToSelf PleaseClose



--


type alias Model msg a =
    { path : FileSystem.Path
    , image : ImageState msg a
    , status : Status
    }


type ImageState msg a
    = NoImage
    | LoadingImage (Queue msg a) (List msg)
    | ReadyImage (Queue msg a) Handle (MemoryImage.FileImage.MemoryImage msg a)


type Handle
    = ReadyHandle FileSystem.Handle.Handle
    | BusyHandle FileSystem.Handle.Handle


handleToHandle : Handle -> FileSystem.Handle.Handle
handleToHandle a =
    case a of
        ReadyHandle b ->
            b

        BusyHandle b ->
            b


type Status
    = Running
    | Exiting



--


init : MemoryImage.FileImage.Config msg a -> FileSystem.Path -> ( Image msg a, Cmd (Msg msg) )
init config path =
    let
        ( image_, cmd ) =
            lifecycle config (Image (Model path NoImage Running))
    in
    ( image_
    , Cmd.batch
        [ cmd
        , Process.Extra.onBeforeExit SaveImage_
        ]
    )



--


type Msg msg
    = GotHandle (Result JavaScript.Error ( FileSystem.Handle.Handle, String ))
    | GotMessage msg
    | QueueDone (Result JavaScript.Error ())
    | FreeHandle
    | SaveImage_
    | PleaseClose
    | NoOperation


update : MemoryImage.FileImage.Config msg a -> (() -> ( a, Cmd msg )) -> (msg -> a -> ( a, Cmd msg )) -> Msg msg -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
update config initFn updateFn msg (Image a) =
    (case msg of
        GotHandle b ->
            case a.image of
                NoImage ->
                    ( Image a
                    , Cmd.none
                    )

                LoadingImage queue messages ->
                    let
                        toDiskImage : ( FileSystem.Handle.Handle, String ) -> Result JavaScript.Error ( FileSystem.Handle.Handle, Maybe (MemoryImage.FileImage.DiskImage msg a) )
                        toDiskImage ( handle, c ) =
                            case c of
                                "" ->
                                    Ok ( handle, Nothing )

                                _ ->
                                    MemoryImage.FileImage.decodeDiskImage config c
                                        |> Result.mapError JavaScript.DecodeError
                                        |> Result.map (\v -> ( handle, Just v ))
                    in
                    case b |> Result.andThen toDiskImage of
                        Ok ( handle, c ) ->
                            let
                                ( image_, cmd ) =
                                    (case c of
                                        Just d ->
                                            ( MemoryImage.FileImage.fromDiskImage updateFn d
                                            , Cmd.none
                                            )

                                        Nothing ->
                                            MemoryImage.FileImage.init initFn
                                    )
                                        |> updateMultiple (MemoryImage.FileImage.update updateFn) (List.reverse messages)

                                nextQueue : Queue msg a
                                nextQueue =
                                    case c of
                                        Just _ ->
                                            queue

                                        Nothing ->
                                            SaveImage
                            in
                            ( Image { a | image = ReadyImage nextQueue (ReadyHandle handle) image_ }
                            , cmd |> Cmd.map GotMessage
                            )

                        Err c ->
                            ( Image a
                            , Console.logError ("Cannot load memory image. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString c)))
                                |> Task.andThen (\_ -> Process.Extra.exit 1)
                                |> Task.attempt (\_ -> NoOperation)
                            )

                ReadyImage _ _ _ ->
                    ( Image a
                    , Cmd.none
                    )

        GotMessage b ->
            case a.image of
                NoImage ->
                    ( Image a
                    , Cmd.none
                    )

                LoadingImage c d ->
                    ( Image { a | image = LoadingImage (queueAddLogMessage b c) (b :: d) }
                    , Cmd.none
                    )

                ReadyImage queue handle image_ ->
                    let
                        ( nextImage, cmd ) =
                            MemoryImage.FileImage.update updateFn b image_
                    in
                    ( Image { a | image = ReadyImage (queueAddLogMessage b queue) handle nextImage }
                    , cmd |> Cmd.map GotMessage
                    )

        QueueDone c ->
            case a.image of
                NoImage ->
                    ( Image a
                    , Cmd.none
                    )

                LoadingImage _ _ ->
                    ( Image a
                    , Cmd.none
                    )

                ReadyImage queue handle image_ ->
                    case c of
                        Ok _ ->
                            ( Image { a | image = ReadyImage queue (ReadyHandle (handleToHandle handle)) image_ }
                            , Cmd.none
                            )

                        Err d ->
                            ( Image { a | image = ReadyImage SaveImage handle image_ }
                            , Cmd.batch
                                [ Console.logError ("Cannot save memory image. " ++ Json.Encode.encode 0 (Json.Encode.string (JavaScript.errorToString d)))
                                    |> Task.attempt (\_ -> NoOperation)
                                , Process.sleep 1000
                                    |> Task.perform (\() -> FreeHandle)
                                ]
                            )

        FreeHandle ->
            case a.image of
                NoImage ->
                    ( Image a
                    , Cmd.none
                    )

                LoadingImage _ _ ->
                    ( Image a
                    , Cmd.none
                    )

                ReadyImage queue handle image_ ->
                    ( Image { a | image = ReadyImage queue (ReadyHandle (handleToHandle handle)) image_ }
                    , Cmd.none
                    )

        SaveImage_ ->
            case a.image of
                NoImage ->
                    ( Image a
                    , Cmd.none
                    )

                LoadingImage _ b ->
                    ( Image { a | image = LoadingImage SaveImage b }
                    , Cmd.none
                    )

                ReadyImage _ handle image_ ->
                    ( Image { a | image = ReadyImage SaveImage handle image_ }
                    , Cmd.none
                    )

        PleaseClose ->
            ( Image { a | status = Exiting }
            , Cmd.none
            )

        NoOperation ->
            ( Image a
            , Cmd.none
            )
    )
        |> (\( v, cmd ) ->
                let
                    ( v2, cmd2 ) =
                        lifecycle config v
                in
                ( v2
                , Cmd.batch [ cmd, cmd2 ]
                )
           )


lifecycle : MemoryImage.FileImage.Config msg a -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
lifecycle config (Image a) =
    case a.status of
        Running ->
            case a.image of
                NoImage ->
                    ( Image { a | image = LoadingImage Empty [] }
                    , getHandle a.path
                        |> Task.attempt GotHandle
                    )

                LoadingImage _ _ ->
                    ( Image a
                    , Cmd.none
                    )

                ReadyImage queue handle image_ ->
                    doQueue config handle image_ queue a

        Exiting ->
            case a.image of
                NoImage ->
                    ( Image a
                    , Cmd.none
                    )

                LoadingImage _ _ ->
                    ( Image a
                    , Cmd.none
                    )

                ReadyImage queue handle image_ ->
                    doQueue config handle image_ queue a


doQueue : MemoryImage.FileImage.Config msg a -> Handle -> MemoryImage.FileImage.MemoryImage msg a -> Queue msg a -> Model msg a -> ( Image msg a, Cmd (Msg msg) )
doQueue config handle image_ queue a =
    case handle of
        ReadyHandle c ->
            case queue of
                Empty ->
                    ( Image a
                    , Cmd.none
                    )

                LogMessage first rest ->
                    let
                        data : String
                        data =
                            (first :: rest)
                                |> List.reverse
                                |> List.map (\v -> v |> config.msgEncoder |> Json.Encode.encode 0 |> (++) "\n")
                                |> String.join ""
                    in
                    ( Image { a | image = ReadyImage Empty (BusyHandle c) image_ }
                    , FileSystem.Handle.write data c
                        |> Task.map (\_ -> ())
                        |> Task.attempt QueueDone
                    )

                SaveImage ->
                    let
                        data : String
                        data =
                            MemoryImage.FileImage.toDiskImage image_ |> MemoryImage.FileImage.encodeDiskImage config
                    in
                    ( Image { a | image = ReadyImage Empty (BusyHandle c) image_ }
                    , FileSystem.Handle.truncate c
                        |> Task.andThen (\_ -> FileSystem.Handle.write data c)
                        |> Task.map (\_ -> ())
                        |> Task.attempt QueueDone
                    )

        BusyHandle _ ->
            ( Image a
            , Cmd.none
            )



--


subscriptions : Image msg a -> Sub (Msg msg)
subscriptions (Image a) =
    case a.status of
        Running ->
            Time.every (1000 * 60 * 60 * 24) (\_ -> SaveImage_)

        Exiting ->
            Sub.none



--


getHandle : FileSystem.Path -> Task.Task JavaScript.Error ( FileSystem.Handle.Handle, String )
getHandle path =
    let
        mode : FileSystem.Handle.Mode
        mode =
            FileSystem.Handle.Mode
                FileSystem.Handle.Read
                FileSystem.Handle.Append
                FileSystem.Handle.CreateIfNotExists
                FileSystem.Handle.DoNotTruncate
    in
    FileSystem.Handle.open mode path
        |> Task.andThen
            (\v ->
                FileSystem.Handle.read v
                    |> Task.map (\v2 -> ( v, v2 ))
            )



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

        LogMessage first rest ->
            LogMessage data (first :: rest)

        SaveImage ->
            SaveImage



--


sendMessageToSelf : a -> Cmd a
sendMessageToSelf a =
    Task.succeed () |> Task.perform (\() -> a)


updateMultiple : (msg -> model -> ( model, Cmd msg )) -> List msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateMultiple updateFn messages ( model, cmd ) =
    messages
        |> List.foldl
            (\msg ( model_, cmds ) ->
                updateFn msg model_
                    |> Tuple.mapSecond (\v -> v :: cmds)
            )
            ( model
            , [ cmd ]
            )
        |> Tuple.mapSecond Cmd.batch
