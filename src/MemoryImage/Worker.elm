module MemoryImage.Worker exposing
    ( Image, image
    , Config, defaultConfig, worker
    , Msg, init, update, updateByMessage, subscriptions
    )

{-|

@docs Image, image

@docs Config, defaultConfig, worker

@docs Msg, init, update, updateByMessage, subscriptions

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
import RunningState
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
    { fileImageConfig : MemoryImage.FileImage.Config msg a

    --
    , init : Json.Decode.Value -> Maybe a -> ( a, Cmd msg )
    , update : msg -> a -> ( a, Cmd msg )
    , subscriptions : a -> Sub msg

    --
    , flagsToImagePath : Json.Decode.Value -> FileSystem.Path
    , mapRunningState : (RunningState.RunningState -> RunningState.RunningState) -> a -> a
    , toRunningState : a -> RunningState.RunningState
    }


defaultConfig :
    MemoryImage.FileImage.Config msg { a | state : RunningState.RunningState }
    -> (Json.Decode.Value -> Maybe { a | state : RunningState.RunningState } -> ( { a | state : RunningState.RunningState }, Cmd msg ))
    -> (msg -> { a | state : RunningState.RunningState } -> ( { a | state : RunningState.RunningState }, Cmd msg ))
    -> ({ a | state : RunningState.RunningState } -> Sub msg)
    -> Config msg { a | state : RunningState.RunningState }
defaultConfig config init_ update_ subscriptions_ =
    Config
        config
        init_
        update_
        subscriptions_
        (\x ->
            x
                |> Json.Decode.decodeValue (Json.Decode.at [ "global", "process", "env", "imagePath" ] Json.Decode.string)
                |> Result.withDefault "image.jsonl"
                |> FileSystem.stringToPath
        )
        (\fn x -> { x | state = fn x.state })
        (\x -> x.state)



--


worker : Config msg a -> Program Json.Decode.Value (Image msg a) (Msg msg)
worker config =
    Platform.worker
        { init = init config
        , update = update config
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
        |> Platform.Extra.andThen ((\(Image x) -> x) >> load config flags >> Tuple.mapFirst Image)



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
    | ImageLoaded Json.Decode.Value (Result JavaScript.Error ( String, FileSystem.Handle.Handle ))
    | MessageReceived msg
    | MessagesSaved (Result JavaScript.Error ())
    | SnapshotSaved (Result JavaScript.Error ())
    | RecoverFromSaveError
    | DayElapsed
    | BeforeExit


update : Config msg a -> Msg msg -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
update config msg (Image model) =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        ImageLoaded b c ->
            imageLoaded config b c model

        MessageReceived b ->
            messageReceived config b model

        MessagesSaved b ->
            messageSaved b model

        SnapshotSaved b ->
            snapshotSaved b model

        RecoverFromSaveError ->
            freeHandle model

        DayElapsed ->
            setSaveMode SaveSnapshot model

        BeforeExit ->
            setSaveMode SaveSnapshot model
    )
        |> Platform.Extra.andThen (save config)
        |> Tuple.mapFirst Image


updateByMessage : Config msg a -> msg -> Image msg a -> ( Image msg a, Cmd (Msg msg) )
updateByMessage config a model =
    update config (MessageReceived a) model



--


load : Config msg a -> Json.Decode.Value -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
load config flags model =
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
            , FileSystem.Handle.open mode (config.flagsToImagePath flags)
                |> Task.andThen
                    (\x ->
                        FileSystem.Handle.read x
                            |> Task.map (\x2 -> ( x2, x ))
                    )
                |> Task.attempt (ImageLoaded flags)
            )

        _ ->
            Platform.Extra.noOperation model


imageLoaded : Config msg a -> Json.Decode.Value -> Result JavaScript.Error ( String, FileSystem.Handle.Handle ) -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
imageLoaded config flags result model =
    let
        toImage : ( String, FileSystem.Handle.Handle ) -> Result JavaScript.Error ( ( a, Cmd msg ), FileSystem.Handle.Handle )
        toImage ( content, handle ) =
            (case content of
                "" ->
                    Ok (config.init flags Nothing)

                _ ->
                    MemoryImage.FileImage.fromString config.fileImageConfig content
                        |> Result.mapError JavaScript.DecodeError
                        |> Result.map
                            (\x ->
                                config.init flags
                                    (x
                                        |> MemoryImage.FileImage.image config.update
                                        |> config.mapRunningState (\_ -> RunningState.Running)
                                        |> Just
                                    )
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


messageReceived : Config msg a -> msg -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
messageReceived config msg model =
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


save : Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
save config model =
    case model.saveMode of
        SaveMessages ->
            saveMessages config model

        SaveSnapshot ->
            saveSnapshot config model


saveMessages : Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
saveMessages config model =
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
                                        |> List.map (\x -> "\n" ++ Json.Encode.encode 0 (config.fileImageConfig.msgEncoder x))
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


saveSnapshot : Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
saveSnapshot config model =
    case model.image of
        Ok a ->
            case a.handle of
                Ok handle ->
                    let
                        data : String
                        data =
                            MemoryImage.FileImage.create [] a.image
                                |> MemoryImage.FileImage.toString config.fileImageConfig
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


messageSaved : Result JavaScript.Error () -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
messageSaved result model =
    case result of
        Ok () ->
            freeHandle model

        Err b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Cannot save messages."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( model
            , Process.sleep 1000
                |> Task.perform (\() -> RecoverFromSaveError)
            )
                |> Platform.Extra.andThen (setSaveMode SaveSnapshot)
                |> Platform.Extra.andThen (log message)


snapshotSaved : Result JavaScript.Error () -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
snapshotSaved result model =
    case result of
        Ok () ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Info
                        "Snapshot saved."
                        Nothing
            in
            freeHandle model
                |> Platform.Extra.andThen (log message)

        Err b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Cannot save snapshot."
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
                [ case config.toRunningState b.image of
                    RunningState.Running ->
                        Time.every (1000 * 60 * 60 * 24) (\_ -> DayElapsed)

                    RunningState.Exiting ->
                        Sub.none
                , config.subscriptions b.image
                    |> Sub.map MessageReceived
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
