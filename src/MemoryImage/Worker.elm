module MemoryImage.Worker exposing
    ( Image, image
    , Config, DailySave(..), worker
    , Msg, init, update, subscriptions, sendMessage
    )

{-|

@docs Image, image

@docs Config, DailySave, worker

@docs Msg, init, update, subscriptions, sendMessage

-}

import Codec
import Console
import FileSystem
import FileSystem.Handle
import JavaScript
import Json.Decode
import Json.Encode
import LogMessage
import MemoryImage.FileImage
import Platform.Extra
import Process
import Process.Extra
import Task
import Task.Extra
import Time


type Image msg a
    = Image (Model msg a)


image : Image msg a -> Maybe a
image (Image a) =
    a.image |> Result.map .image |> Result.toMaybe



--


type alias Config msg a =
    { init : Maybe a -> ( a, Cmd msg )
    , update : msg -> a -> ( a, Cmd msg )
    , subscriptions : a -> Sub msg
    , flagsToImagePath : Json.Decode.Value -> FileSystem.Path
    , toDailySave : a -> DailySave
    }



--


type DailySave
    = DailySave
    | NoDailySave



--


worker : Config msg a -> MemoryImage.FileImage.Config msg a -> Program Json.Decode.Value (Image msg a) (Msg msg)
worker config config2 =
    Platform.worker
        { init = init config
        , update = update config config2
        , subscriptions = subscriptions config
        }



--


type alias Model msg a =
    { image : Result Error (ReadyImage a)
    , saveQueue : List msg
    , saveMode : SaveMode
    }


init : Config msg a -> Json.Decode.Value -> ( Image msg a, Cmd (Msg msg) )
init config flags =
    ( Image
        (Model
            (Err NoImage)
            []
            SaveMessages
        )
    , Process.Extra.onBeforeExit BeforeExit
    )
        |> Platform.Extra.andThen ((\(Image x) -> x) >> load (config.flagsToImagePath flags) >> Tuple.mapFirst Image)



--


type alias ReadyImage a =
    { image : a
    , handle : Result FileSystem.Handle.Handle FileSystem.Handle.Handle
    }



--


type Error
    = NoImage
    | Loading
    | JavaScriptError JavaScript.Error



--


type SaveMode
    = SaveMessages
    | SaveSnapshot



--


type Msg msg
    = NothingHappened
    | ImageLoaded (Result JavaScript.Error ( String, FileSystem.Handle.Handle ))
    | MessageReceived msg
    | MessagesSaved (Result JavaScript.Error ())
    | SnapshotSaved (Result JavaScript.Error ())
    | RecoverFromSaveError
    | DayElapsed
    | BeforeExit


update : Config msg a -> MemoryImage.FileImage.Config msg a -> Msg msg -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
update config config2 msg (Image model) =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        ImageLoaded b ->
            imageLoaded config config2 b model

        MessageReceived b ->
            messageReceived config config2 b model

        MessagesSaved b ->
            queueSaved b model

        SnapshotSaved b ->
            queueSaved b model

        RecoverFromSaveError ->
            freeHandle model

        DayElapsed ->
            setSaveMode SaveSnapshot model

        BeforeExit ->
            setSaveMode SaveSnapshot model
    )
        |> Platform.Extra.andThen (save config config2)
        |> Tuple.mapFirst Image


sendMessage : Config msg a -> MemoryImage.FileImage.Config msg a -> msg -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
sendMessage config config2 a model =
    update config config2 (MessageReceived a) model



--


load : FileSystem.Path -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
load path model =
    case model.image of
        Err NoImage ->
            let
                mode : FileSystem.Handle.Mode
                mode =
                    FileSystem.Handle.Mode
                        FileSystem.Handle.Read
                        FileSystem.Handle.Append
                        FileSystem.Handle.CreateIfNotExists
                        FileSystem.Handle.DoNotTruncate
            in
            ( { model
                | image = Err Loading
              }
            , FileSystem.Handle.open mode path
                |> Task.andThen
                    (\x ->
                        FileSystem.Handle.read x
                            |> Task.map (\x2 -> ( x2, x ))
                    )
                |> Task.attempt ImageLoaded
            )

        _ ->
            Platform.Extra.noOperation model


imageLoaded : Config msg a -> MemoryImage.FileImage.Config msg a -> Result JavaScript.Error ( String, FileSystem.Handle.Handle ) -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
imageLoaded config config2 result model =
    let
        toImage : ( String, FileSystem.Handle.Handle ) -> Result JavaScript.Error ( ( a, Cmd msg ), FileSystem.Handle.Handle )
        toImage ( content, handle ) =
            (case content of
                "" ->
                    Ok (config.init Nothing)

                _ ->
                    MemoryImage.FileImage.fromString config2 content
                        |> Result.mapError JavaScript.DecodeError
                        |> Result.map
                            (\x ->
                                config.init (Just (MemoryImage.FileImage.image config.update x))
                            )
            )
                |> Result.map (\x -> ( x, handle ))
    in
    case result |> Result.andThen toImage of
        Ok ( a, handle ) ->
            let
                ( image_, cmd ) =
                    replayMessages config (List.reverse model.saveQueue) a
            in
            ( { model
                | image = Ok { image = image_, handle = Ok handle }
              }
            , cmd |> Cmd.map MessageReceived
            )

        Err b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Cannot load image."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( { model
                | image = Err (JavaScriptError b)
              }
            , logMessage message
                |> Task.Extra.andAlwaysThen (\_ -> Process.Extra.exit 1)
                |> Task.attempt (\_ -> NothingHappened)
            )



--


messageReceived : Config msg a -> MemoryImage.FileImage.Config msg a -> msg -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
messageReceived config _ msg model =
    (case model.image of
        Ok a ->
            let
                ( nextImage, cmd ) =
                    config.update msg a.image
            in
            ( { model
                | image = Ok { a | image = nextImage }
              }
            , cmd |> Cmd.map MessageReceived
            )

        Err _ ->
            Platform.Extra.noOperation model
    )
        |> Platform.Extra.andThen
            (\x ->
                ( { x | saveQueue = msg :: x.saveQueue }
                , Cmd.none
                )
            )



--


save : Config msg a -> MemoryImage.FileImage.Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
save config config2 model =
    case model.saveMode of
        SaveMessages ->
            saveMessages config config2 model

        SaveSnapshot ->
            saveSnapshot config config2 model


saveMessages : Config msg a -> MemoryImage.FileImage.Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
saveMessages _ config2 model =
    case model.image of
        Ok a ->
            case a.handle of
                Ok handle ->
                    let
                        data : Maybe String
                        data =
                            case model.saveQueue of
                                [] ->
                                    Nothing

                                _ ->
                                    model.saveQueue
                                        |> List.reverse
                                        |> List.map (\x -> "\n" ++ Json.Encode.encode 0 (config2.msgEncoder x))
                                        |> String.join ""
                                        |> Just
                    in
                    case data of
                        Just b ->
                            ( { model
                                | image = Ok { a | handle = Err handle }
                                , saveQueue = []
                                , saveMode = SaveMessages
                              }
                            , FileSystem.Handle.write b handle
                                |> Task.attempt MessagesSaved
                            )

                        Nothing ->
                            Platform.Extra.noOperation model

                Err _ ->
                    Platform.Extra.noOperation model

        _ ->
            Platform.Extra.noOperation model


saveSnapshot : Config msg a -> MemoryImage.FileImage.Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
saveSnapshot _ config2 model =
    case model.image of
        Ok a ->
            case a.handle of
                Ok handle ->
                    let
                        data : String
                        data =
                            MemoryImage.FileImage.create [] a.image
                                |> MemoryImage.FileImage.toString config2
                    in
                    ( { model
                        | image = Ok { a | handle = Err handle }
                        , saveQueue = []
                        , saveMode = SaveMessages
                      }
                    , FileSystem.Handle.truncate handle
                        |> Task.andThen (\_ -> FileSystem.Handle.write data handle)
                        |> Task.attempt SnapshotSaved
                    )

                Err _ ->
                    Platform.Extra.noOperation model

        _ ->
            Platform.Extra.noOperation model


queueSaved : Result JavaScript.Error () -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
queueSaved result model =
    case result of
        Ok () ->
            freeHandle model

        Err b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Cannot save image."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( model
            , Process.sleep 1000
                |> Task.perform (\() -> RecoverFromSaveError)
            )
                |> Platform.Extra.andThen (setSaveMode SaveSnapshot)
                |> Platform.Extra.andThen (log message)


freeHandle : Model msg a -> ( Model msg a, Cmd (Msg msg) )
freeHandle model =
    case model.image of
        Ok a ->
            let
                handle : FileSystem.Handle.Handle
                handle =
                    case a.handle of
                        Ok b ->
                            b

                        Err b ->
                            b
            in
            ( { model
                | image = Ok { a | handle = Ok handle }
              }
            , Cmd.none
            )

        Err _ ->
            Platform.Extra.noOperation model



--


setSaveMode : SaveMode -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
setSaveMode a model =
    ( { model | saveMode = a }
    , Cmd.none
    )


log : LogMessage.LogMessage -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
log a model =
    ( model
    , logMessage a
        |> Task.attempt (\_ -> NothingHappened)
    )



--


subscriptions : Config msg a -> Image msg a -> Sub (Msg msg)
subscriptions config (Image a) =
    case a.image of
        Ok b ->
            Sub.batch
                [ case config.toDailySave b.image of
                    DailySave ->
                        Time.every (1000 * 60 * 60 * 24) (\_ -> DayElapsed)

                    NoDailySave ->
                        Sub.none
                , config.subscriptions b.image |> Sub.map MessageReceived
                ]

        Err _ ->
            Sub.none



--


replayMessages : Config msg a -> List msg -> ( a, Cmd msg ) -> ( a, Cmd msg )
replayMessages config messages ( a, cmd ) =
    let
        fn : msg -> ( a, List (Cmd msg) ) -> ( a, List (Cmd msg) )
        fn msg ( x, cmds ) =
            let
                ( next, cmd_ ) =
                    config.update msg x
            in
            ( next
            , cmd_ :: cmds
            )
    in
    messages
        |> List.foldl fn ( a, [ cmd ] )
        |> Tuple.mapSecond Cmd.batch


logMessage : LogMessage.LogMessage -> Task.Task JavaScript.Error ()
logMessage a =
    let
        fn : String -> Task.Task JavaScript.Error ()
        fn =
            case a.type_ of
                LogMessage.Info ->
                    Console.logInfo

                LogMessage.Warning ->
                    Console.logWarning

                LogMessage.Error ->
                    Console.logError
    in
    fn (Codec.encodeToString 0 LogMessage.codec a)
