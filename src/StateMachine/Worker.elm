module StateMachine.Worker exposing
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
import Platform.Extra
import Process
import Process.Extra
import StateMachine.File
import StateMachine.RunningState
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
    { init : () -> a
    , update : msg -> a -> ( a, Cmd msg )
    , subscriptions : a -> Sub msg

    --
    , flagsToImagePath : Json.Decode.Value -> FileSystem.Path
    , mapRunningState : (StateMachine.RunningState.RunningState -> StateMachine.RunningState.RunningState) -> a -> a
    , toRunningState : a -> StateMachine.RunningState.RunningState

    --
    , flagsReceived : Json.Decode.Value -> msg
    }


defaultConfig :
    (() -> { a | state : StateMachine.RunningState.RunningState })
    -> (msg -> { a | state : StateMachine.RunningState.RunningState } -> ( { a | state : StateMachine.RunningState.RunningState }, Cmd msg ))
    -> ({ a | state : StateMachine.RunningState.RunningState } -> Sub msg)
    -> (Json.Decode.Value -> msg)
    -> Config msg { a | state : StateMachine.RunningState.RunningState }
defaultConfig init_ update_ subscriptions_ flagsReceived =
    Config
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
        flagsReceived



--


worker : Config msg a -> Program Json.Decode.Value (Image msg a) (Msg a msg)
worker config =
    Platform.worker
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
        }



--


type alias Model msg a =
    { image : Result Error (ReadyImage a)
    , imagePath : FileSystem.Path
    , saveQueue : List msg
    , saveMode : SaveMode
    }


init : Config msg a -> Json.Decode.Value -> ( Image msg a, Cmd (Msg a msg) )
init config flags =
    ( Image
        (Model
            (Err NoImage)
            (config.flagsToImagePath flags)
            [ config.flagsReceived flags ]
            SaveMessages
        )
    , Process.Extra.onBeforeExit BeforeExit
    )
        |> Platform.Extra.andThen ((\(Image x) -> x) >> load config >> Tuple.mapFirst Image)



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


type Msg a msg
    = NothingHappened
    | ImageLoaded (Result JavaScript.Error ( Maybe a, FileSystem.Handle.Handle ))
    | MessageReceived msg
    | MessagesSaved (Result JavaScript.Error ())
    | SnapshotSaved (Result JavaScript.Error FileSystem.Handle.Handle)
    | RecoverFromSaveError
    | DayElapsed
    | BeforeExit


update : Config msg a -> Msg a msg -> Image msg a -> ( Image msg a, Cmd (Msg a msg) )
update config msg (Image model) =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        ImageLoaded b ->
            imageLoaded config b model

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


updateByMessage : Config msg a -> msg -> Image msg a -> ( Image msg a, Cmd (Msg a msg) )
updateByMessage config a model =
    update config (MessageReceived a) model



--


load : Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
load config model =
    let
        toImage : String -> Result Json.Decode.Error (Maybe a)
        toImage b =
            case b of
                "" ->
                    Ok Nothing

                _ ->
                    StateMachine.File.fromString config.fileImageConfig b
                        |> Result.map
                            (\x ->
                                x
                                    |> StateMachine.File.state config.update
                                    |> config.mapRunningState (\_ -> StateMachine.RunningState.Running)
                                    |> Just
                            )
    in
    case model.image of
        Err NoImage ->
            ( { model
                | image = Err Loading
              }
            , FileSystem.Handle.open fileMode model.imagePath
                |> Task.andThen
                    (\handle ->
                        FileSystem.Handle.read handle
                            |> Task.andThen (toImage >> Task.Extra.fromResult >> Task.mapError JavaScript.DecodeError)
                            |> Task.Extra.andAlwaysThen
                                (\x ->
                                    case x of
                                        Ok x2 ->
                                            Task.succeed ( x2, handle )

                                        Err x2 ->
                                            FileSystem.Handle.close handle
                                                |> Task.Extra.andAlwaysThen (\_ -> Task.fail x2)
                                )
                    )
                |> Task.attempt ImageLoaded
            )

        _ ->
            Platform.Extra.noOperation model


imageLoaded : Config msg a -> Result JavaScript.Error ( Maybe a, FileSystem.Handle.Handle ) -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
imageLoaded config result model =
    case result of
        Ok ( a, handle ) ->
            let
                ( image_, cmd ) =
                    ( case a of
                        Just b ->
                            b

                        Nothing ->
                            config.init ()
                    , Cmd.none
                    )
                        |> replayMessages config (List.reverse model.saveQueue)

                saveMode : SaveMode
                saveMode =
                    case a of
                        Just _ ->
                            SaveMessages

                        Nothing ->
                            SaveSnapshot

                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Info
                        "Memory Image"
                        (case a of
                            Just _ ->
                                "Image was loaded."

                            Nothing ->
                                "Image was initialized."
                        )
                        Nothing
            in
            ( { model
                | image = Ok { image = image_, handle = Ok handle }
                , saveMode = saveMode
              }
            , cmd |> Cmd.map MessageReceived
            )
                |> Platform.Extra.andThen (log message)

        Err b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Memory Image"
                        "Cannot load image."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( { model
                | image = Err (JavaScriptError b)
              }
            , Process.Extra.softExit
                |> Task.attempt (\_ -> NothingHappened)
            )
                |> Platform.Extra.andThen (log message)



--


messageReceived : Config msg a -> msg -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
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


save : Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
save config model =
    case model.saveMode of
        SaveMessages ->
            saveMessages config model

        SaveSnapshot ->
            saveSnapshot config model


saveMessages : Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
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


saveSnapshot : Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
saveSnapshot config model =
    case model.image of
        Ok a ->
            case a.handle of
                Ok handle ->
                    let
                        data : String
                        data =
                            StateMachine.File.create [] a.image
                                |> StateMachine.File.toString config.fileImageConfig

                        tmpPath : FileSystem.Path
                        tmpPath =
                            model.imagePath
                                |> FileSystem.pathToString
                                |> (\x -> x ++ ".tmp")
                                |> FileSystem.stringToPath
                    in
                    ( { model
                        | image = Ok { a | handle = Err handle }
                        , saveQueue = []
                        , saveMode = SaveMessages
                      }
                    , FileSystem.Handle.open fileMode tmpPath
                        |> Task.andThen
                            (\newHandle ->
                                FileSystem.Handle.write data newHandle
                                    |> Task.andThen (\() -> FileSystem.rename tmpPath model.imagePath)
                                    |> Task.Extra.andAlwaysThen
                                        (\x ->
                                            case x of
                                                Ok () ->
                                                    FileSystem.Handle.close handle
                                                        |> Task.Extra.andAlwaysThen (\_ -> Task.succeed newHandle)

                                                Err x2 ->
                                                    FileSystem.Handle.close newHandle
                                                        |> Task.Extra.andAlwaysThen (\_ -> Task.fail x2)
                                        )
                            )
                        |> Task.attempt SnapshotSaved
                    )

                Err _ ->
                    Platform.Extra.noOperation model

        _ ->
            Platform.Extra.noOperation model


messageSaved : Result JavaScript.Error () -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
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
                        "Memory Image"
                        "Cannot save messages."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( model
            , Process.sleep 1000
                |> Task.perform (\() -> RecoverFromSaveError)
            )
                |> Platform.Extra.andThen (setSaveMode SaveSnapshot)
                |> Platform.Extra.andThen (log message)


snapshotSaved : Result JavaScript.Error FileSystem.Handle.Handle -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
snapshotSaved result model =
    case result of
        Ok handle ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Info
                        "Memory Image"
                        "Snapshot saved."
                        Nothing
            in
            (case model.image of
                Ok a ->
                    ( { model
                        | image = Ok { a | handle = Ok handle }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    Platform.Extra.noOperation model
            )
                |> Platform.Extra.andThen (log message)

        Err b ->
            let
                message : LogMessage.LogMessage
                message =
                    LogMessage.LogMessage
                        LogMessage.Error
                        "Memory Image"
                        "Cannot save snapshot."
                        (Just (LogMessage.JavaScriptError b))
            in
            ( model
            , Process.sleep 1000
                |> Task.perform (\() -> RecoverFromSaveError)
            )
                |> Platform.Extra.andThen (setSaveMode SaveSnapshot)
                |> Platform.Extra.andThen (log message)


freeHandle : Model msg a -> ( Model msg a, Cmd (Msg a msg) )
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


setSaveMode : SaveMode -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
setSaveMode a model =
    ( { model | saveMode = a }
    , Cmd.none
    )


log : LogMessage.LogMessage -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
log a model =
    ( model
    , logMessage a
        |> Task.attempt (\_ -> NothingHappened)
    )



--


subscriptions : Config msg a -> Image msg a -> Sub (Msg a msg)
subscriptions config (Image a) =
    case a.image of
        Ok b ->
            Sub.batch
                [ case config.toRunningState b.image of
                    StateMachine.RunningState.Running ->
                        Time.every (1000 * 60 * 60 * 24) (\_ -> DayElapsed)

                    StateMachine.RunningState.Exiting ->
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


fileMode : FileSystem.Handle.Mode
fileMode =
    FileSystem.Handle.Mode
        FileSystem.Handle.Read
        FileSystem.Handle.Append
        FileSystem.Handle.CreateIfNotExists
        FileSystem.Handle.DoNotTruncate
