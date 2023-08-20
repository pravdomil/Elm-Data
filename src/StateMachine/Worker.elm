module StateMachine.Worker exposing
    ( Config, flagsToFilePath, worker
    , init, update, updateByMessage, subscriptions
    )

{-|

@docs Config, flagsToFilePath, worker

@docs init, update, updateByMessage, subscriptions

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
import StateMachine.Lifecycle
import Task
import Task.Extra
import Time


type alias Config msg a =
    { init : () -> ( a, Cmd msg )
    , update : msg -> a -> ( a, Cmd msg )
    , subscriptions : a -> Sub msg

    --
    , codec : Codec.Codec a
    , msgCodec : Codec.Codec msg

    --
    , flagsToFilePath : Json.Decode.Value -> FileSystem.Path
    , flagsReceived : Json.Decode.Value -> msg

    --
    , toLifecycle : a -> StateMachine.Lifecycle.Lifecycle
    , lifecycleChanged : StateMachine.Lifecycle.Lifecycle -> msg
    }


flagsToFilePath : Json.Decode.Value -> FileSystem.Path
flagsToFilePath a =
    FileSystem.stringToPath
        (Result.withDefault "state_machine.jsonl"
            (Json.Decode.decodeValue (Json.Decode.at [ "global", "process", "env", "stateMachineFilePath" ] Json.Decode.string) a)
        )



--


worker : Config msg a -> Program Json.Decode.Value (Model msg a) (Msg a msg)
worker config =
    Platform.worker
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
        }



--


type alias Model msg a =
    { filePath : FileSystem.Path
    , state : Result (StateError a) (State a)
    , saveQueue : List msg
    }



--


type alias State a =
    { state : a
    , handle : Result FileSystem.Handle.Handle FileSystem.Handle.Handle
    , saveMode : SaveMode
    }



--


type StateError a
    = NotLoaded
    | Loading
    | JavaScriptError JavaScript.Error
    | Exiting (State a)



--


type SaveMode
    = SaveMessages
    | SaveState



--


type Msg a msg
    = NothingHappened
      --
    | StateLoaded (Result JavaScript.Error ( Maybe a, FileSystem.Handle.Handle ))
    | MessageReceived msg
      --
    | MessagesSaved (Result JavaScript.Error ())
    | StateSaved (Result JavaScript.Error FileSystem.Handle.Handle)
    | RecoverFromSaveError
      --
    | DayElapsed
    | ExitRequested



--


init : Config msg a -> Json.Decode.Value -> ( Model msg a, Cmd (Msg a msg) )
init config flags =
    ( Model
        (config.flagsToFilePath flags)
        (Err NotLoaded)
        [ config.lifecycleChanged StateMachine.Lifecycle.Running
        , config.flagsReceived flags
        ]
    , Cmd.none
    )
        |> Platform.Extra.andThen
            (\x ->
                ( x, Process.Extra.onInterruptAndTerminationSignal (MessageReceived (config.lifecycleChanged StateMachine.Lifecycle.Exiting)) )
            )
        |> Platform.Extra.andThen
            (\x ->
                ( x, Process.Extra.onBeforeExit ExitRequested )
            )
        |> Platform.Extra.andThen (load config)


update : Config msg a -> Msg a msg -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
update config msg =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation

        StateLoaded b ->
            stateLoaded config b

        MessageReceived b ->
            messageReceived config b

        MessagesSaved b ->
            messageSaved b

        StateSaved b ->
            stateSaved b

        RecoverFromSaveError ->
            freeHandle

        DayElapsed ->
            setSaveMode SaveState

        ExitRequested ->
            exitRequested
    )
        >> Platform.Extra.andThen (save config)


updateByMessage : Config msg a -> msg -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
updateByMessage config a model =
    update config (MessageReceived a) model


subscriptions : Config msg a -> Model msg a -> Sub (Msg a msg)
subscriptions config a =
    case a.state of
        Ok b ->
            Sub.batch
                [ case config.toLifecycle b.state of
                    StateMachine.Lifecycle.Running ->
                        Time.every (1000 * 60 * 60 * 24) (\_ -> DayElapsed)

                    StateMachine.Lifecycle.Exiting ->
                        Sub.none
                , config.subscriptions b.state
                    |> Sub.map MessageReceived
                ]

        Err _ ->
            Sub.none



--


load : Config msg a -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
load config model =
    let
        toState : String -> Result Json.Decode.Error (Maybe a)
        toState b =
            case b of
                "" ->
                    Ok Nothing

                _ ->
                    StateMachine.File.fromString config.codec config.msgCodec b
                        |> Result.map (\x -> Just (StateMachine.File.state config.update x))
    in
    case model.state of
        Err NotLoaded ->
            ( { model | state = Err Loading }
            , FileSystem.Handle.open fileMode model.filePath
                |> Task.andThen
                    (\handle ->
                        FileSystem.Handle.read handle
                            |> Task.andThen (toState >> Task.Extra.fromResult >> Task.mapError JavaScript.DecodeError)
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
                |> Task.attempt StateLoaded
            )

        _ ->
            Platform.Extra.noOperation model


stateLoaded : Config msg a -> Result JavaScript.Error ( Maybe a, FileSystem.Handle.Handle ) -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
stateLoaded config result model =
    case result of
        Ok ( a, handle ) ->
            case a of
                Just b ->
                    let
                        ( state, cmd ) =
                            replayMessages config (List.reverse model.saveQueue) ( b, Cmd.none )
                    in
                    ( { model
                        | state = Ok (State state (Ok handle) SaveMessages)
                      }
                    , cmd |> Cmd.map MessageReceived
                    )
                        |> Platform.Extra.andThen
                            (\x ->
                                log (LogMessage.LogMessage LogMessage.Info "State Machine" "State was loaded." Nothing) x
                            )

                Nothing ->
                    let
                        ( state, cmd ) =
                            replayMessages config (List.reverse model.saveQueue) (config.init ())
                    in
                    ( { model
                        | state = Ok (State state (Ok handle) SaveState)
                      }
                    , cmd |> Cmd.map MessageReceived
                    )
                        |> Platform.Extra.andThen
                            (\x ->
                                log (LogMessage.LogMessage LogMessage.Info "State Machine" "State was initialized." Nothing) x
                            )

        Err b ->
            ( { model
                | state = Err (JavaScriptError b)
              }
            , Process.Extra.softExit
                |> Task.attempt (\_ -> NothingHappened)
            )
                |> Platform.Extra.andThen
                    (\x ->
                        log (LogMessage.LogMessage LogMessage.Error "State Machine" "Cannot load state." (Just (LogMessage.JavaScriptError b))) x
                    )



--


messageReceived : Config msg a -> msg -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
messageReceived config msg model =
    (case model.state of
        Ok a ->
            let
                ( state, cmd ) =
                    config.update msg a.state
            in
            ( { model
                | state = Ok { a | state = state }
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
    case model.state of
        Ok b ->
            case b.saveMode of
                SaveMessages ->
                    saveMessages config b model

                SaveState ->
                    saveState config b model

        Err (Exiting b) ->
            saveState config b model


saveMessages : Config msg a -> State a -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
saveMessages config a model =
    case a.handle of
        Ok handle ->
            case model.saveQueue of
                [] ->
                    Platform.Extra.noOperation model

                _ ->
                    let
                        data : String
                        data =
                            String.join ""
                                (List.map
                                    (\x -> "\n" ++ Codec.encodeToString 0 config.msgCodec x)
                                    (List.reverse model.saveQueue)
                                )
                    in
                    ( { model
                        | state = Ok { a | handle = Err handle, saveMode = SaveMessages }
                        , saveQueue = []
                      }
                    , FileSystem.Handle.write data handle
                        |> Task.attempt MessagesSaved
                    )

        Err _ ->
            Platform.Extra.noOperation model


saveState : Config msg a -> State a -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
saveState config a model =
    case a.handle of
        Ok handle ->
            let
                data : String
                data =
                    StateMachine.File.create [] a.state
                        |> StateMachine.File.toString config.codec config.msgCodec

                tmpPath : FileSystem.Path
                tmpPath =
                    FileSystem.stringToPath (FileSystem.pathToString model.filePath ++ ".tmp")
            in
            ( { model
                | state = Ok { a | handle = Err handle, saveMode = SaveMessages }
                , saveQueue = []
              }
            , FileSystem.Handle.open fileMode tmpPath
                |> Task.andThen
                    (\newHandle ->
                        FileSystem.Handle.write data newHandle
                            |> Task.andThen (\() -> FileSystem.rename tmpPath model.filePath)
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
                |> Task.attempt StateSaved
            )

        Err _ ->
            Platform.Extra.noOperation model


messageSaved : Result JavaScript.Error () -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
messageSaved result model =
    case result of
        Ok () ->
            freeHandle model

        Err b ->
            ( { model | state = Result.map (\x -> { x | saveMode = SaveState }) model.state }
            , Process.sleep 1000
                |> Task.perform (\() -> RecoverFromSaveError)
            )
                |> Platform.Extra.andThen
                    (\x ->
                        log (LogMessage.LogMessage LogMessage.Error "State Machine" "Cannot save messages." (Just (LogMessage.JavaScriptError b))) x
                    )


stateSaved : Result JavaScript.Error FileSystem.Handle.Handle -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
stateSaved result model =
    case result of
        Ok handle ->
            (case model.state of
                Ok a ->
                    ( { model
                        | state = Ok { a | handle = Ok handle }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    Platform.Extra.noOperation model
            )
                |> Platform.Extra.andThen
                    (\x ->
                        log (LogMessage.LogMessage LogMessage.Info "State Machine" "State saved." Nothing) x
                    )

        Err b ->
            ( { model | state = Result.map (\x -> { x | saveMode = SaveState }) model.state }
            , Process.sleep 1000
                |> Task.perform (\() -> RecoverFromSaveError)
            )
                |> Platform.Extra.andThen
                    (\x ->
                        log (LogMessage.LogMessage LogMessage.Error "State Machine" "Cannot save state." (Just (LogMessage.JavaScriptError b))) x
                    )


freeHandle : Model msg a -> ( Model msg a, Cmd (Msg a msg) )
freeHandle model =
    case model.state of
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
                | state = Ok { a | handle = Ok handle }
              }
            , Cmd.none
            )

        Err _ ->
            Platform.Extra.noOperation model


exitRequested : Model msg a -> ( Model msg a, Cmd (Msg a msg) )
exitRequested model =
    case model.state of
        Ok a ->
            ( { model
                | state = Err (Exiting a)
              }
            , Cmd.none
            )

        Err _ ->
            Platform.Extra.noOperation model



--


setSaveMode : SaveMode -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
setSaveMode a model =
    ( { model | state = Result.map (\x -> { x | saveMode = a }) model.state }
    , Cmd.none
    )


log : LogMessage.LogMessage -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
log a model =
    ( model
    , logMessage a
        |> Task.attempt (\_ -> NothingHappened)
    )



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
    List.foldl fn ( a, [ cmd ] ) messages
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
