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
import MemoryImage
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
            Just (MemoryImage.image b)


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
    | LoadingImage (Maybe (Queue msg a)) (List msg)
    | ReadyImage (Maybe (Queue msg a)) Handle (MemoryImage.MemoryImage msg a)


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


init : MemoryImage.Config msg a -> FileSystem.Path -> ( Image msg a, Cmd (Msg msg) )
init config path =
    lifecycle config (Image (Model path NoImage Running))



--


type Msg msg
    = GotHandle (Result JavaScript.Error ( FileSystem.Handle.Handle, String ))
    | GotMessage msg
    | QueueDone (Result JavaScript.Error ())
    | DayElapsed
    | PleaseClose
    | NoOperation


update : MemoryImage.Config msg a -> (() -> ( a, Cmd msg )) -> (msg -> a -> ( a, Cmd msg )) -> Msg msg -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
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
                            let
                                ( image_, cmd ) =
                                    (case c of
                                        Just d ->
                                            ( MemoryImage.diskImageToMemoryImage updateFn d
                                            , Cmd.none
                                            )

                                        Nothing ->
                                            MemoryImage.init initFn
                                    )
                                        |> updateMultiple (MemoryImage.update updateFn) messages
                            in
                            ( Image { a | image = ReadyImage queue (ReadyHandle handle) image_ }
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
                            MemoryImage.update updateFn b image_
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
                            ( Image { a | image = ReadyImage (Just SaveImage) (ReadyHandle (handleToHandle handle)) image_ }
                            , Console.logError ("Cannot save memory image. See details:\n" ++ JavaScript.errorToString d)
                                |> Task.attempt (\_ -> NoOperation)
                            )

        DayElapsed ->
            case a.image of
                NoImage ->
                    ( Image a
                    , Cmd.none
                    )

                LoadingImage _ b ->
                    ( Image { a | image = LoadingImage (Just SaveImage) b }
                    , Cmd.none
                    )

                ReadyImage _ handle memoryImage ->
                    ( Image { a | image = ReadyImage (Just SaveImage) handle memoryImage }
                    , Cmd.none
                    )

        PleaseClose ->
            case a.image of
                NoImage ->
                    ( Image { a | status = Exiting }
                    , Cmd.none
                    )

                LoadingImage _ b ->
                    ( Image { a | status = Exiting, image = LoadingImage (Just SaveImage) b }
                    , Cmd.none
                    )

                ReadyImage _ b c ->
                    ( Image { a | status = Exiting, image = ReadyImage (Just SaveImage) b c }
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


lifecycle : MemoryImage.Config msg a -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
lifecycle config (Image a) =
    case a.status of
        Running ->
            case a.image of
                NoImage ->
                    ( Image { a | image = LoadingImage Nothing [] }
                    , getHandle a.path
                        |> Task.attempt GotHandle
                    )

                LoadingImage _ _ ->
                    ( Image a
                    , Cmd.none
                    )

                ReadyImage queue handle image_ ->
                    case queue of
                        Just c ->
                            doQueue config handle image_ c a

                        Nothing ->
                            ( Image a
                            , Cmd.none
                            )

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
                    case queue of
                        Just b ->
                            doQueue config handle image_ b a

                        Nothing ->
                            case handle of
                                ReadyHandle c ->
                                    ( Image { a | image = NoImage }
                                    , FileSystem.Handle.close c
                                        |> Task.attempt (\_ -> NoOperation)
                                    )

                                BusyHandle _ ->
                                    ( Image a
                                    , Cmd.none
                                    )


doQueue : MemoryImage.Config msg a -> Handle -> MemoryImage.MemoryImage msg a -> Queue msg a -> Model msg a -> ( Image msg a, Cmd (Msg msg) )
doQueue config handle image_ queue a =
    case handle of
        ReadyHandle c ->
            case queue of
                LogMessage first rest ->
                    let
                        data : String
                        data =
                            encodeMessages config (first :: rest)
                    in
                    ( Image { a | image = ReadyImage Nothing (BusyHandle c) image_ }
                    , FileSystem.Handle.write data c
                        |> Task.map (\_ -> ())
                        |> Task.attempt QueueDone
                    )

                SaveImage ->
                    let
                        data : String
                        data =
                            Json.Encode.encode 0 (config.encoder (MemoryImage.image image_))
                    in
                    ( Image { a | image = ReadyImage Nothing (BusyHandle c) image_ }
                    , FileSystem.Handle.truncate c
                        |> Task.andThen (FileSystem.Handle.write data)
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
            Time.every (1000 * 60 * 60 * 24) (\_ -> DayElapsed)

        Exiting ->
            Sub.none



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


type Queue msg a
    = LogMessage msg (List msg)
    | SaveImage


queueAddLogMessage : msg -> Maybe (Queue msg a) -> Maybe (Queue msg a)
queueAddLogMessage data a =
    case a of
        Just b ->
            case b of
                LogMessage first rest ->
                    Just (LogMessage data (first :: rest))

                SaveImage ->
                    Just SaveImage

        Nothing ->
            Just (LogMessage data [])


encodeMessages : MemoryImage.Config msg a -> List msg -> String
encodeMessages config a =
    a
        |> List.reverse
        |> List.map (\v -> v |> config.msgEncoder |> Json.Encode.encode 0 |> (++) "\n")
        |> String.join ""



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
