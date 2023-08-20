module StateMachine.Server exposing
    ( worker, init, update, subscriptions
    , Config, flagsToServerOptions
    )

{-|

@docs worker, init, update, subscriptions

@docs Config, flagsToServerOptions

-}

import Codec
import FileSystem
import Http.Server
import Http.Server.Worker
import Json.Decode
import Platform.Extra
import Process.Extra
import StateMachine.Lifecycle
import StateMachine.Worker


worker : Config msg a -> Program Json.Decode.Value (Model msg a) (Msg a msg)
worker config =
    Platform.worker
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
        }



--


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

    --
    , flagsToServerOptions : Json.Decode.Value -> Http.Server.Options
    , requestReceived : Http.Server.Request -> msg
    }


flagsToServerOptions : Json.Decode.Value -> Http.Server.Options
flagsToServerOptions a =
    Json.Decode.decodeValue (Json.Decode.at [ "global", "process", "env", "serverOptions" ] Json.Decode.string) a
        |> Result.andThen (Json.Decode.decodeString Http.Server.optionsDecoder)
        |> Result.withDefault Http.Server.emptyOptions



--


type alias Model msg a =
    { server : Http.Server.Worker.Worker
    , state : StateMachine.Worker.Model msg a
    }


init : Config msg a -> Json.Decode.Value -> ( Model msg a, Cmd (Msg a msg) )
init config flags =
    let
        ( server, cmd ) =
            Http.Server.Worker.init (config.flagsToServerOptions flags)

        ( image, cmd2 ) =
            StateMachine.Worker.init config.stateMachineConfig flags
    in
    ( Model server image
    , Cmd.batch
        [ cmd |> Cmd.map ServerMessageReceived
        , cmd2 |> Cmd.map ImageMessageReceived
        , Process.Extra.onInterruptAndTerminationSignal ExitRequested
        ]
    )



--


type Msg a msg
    = ServerMessageReceived Http.Server.Worker.Msg
    | ImageMessageReceived (StateMachine.Worker.Msg a msg)
    | ExitRequested


update : Config msg a -> Msg a msg -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
update config msg model =
    case msg of
        ServerMessageReceived b ->
            updateServer b model
                |> Platform.Extra.andThen
                    (case Http.Server.Worker.toPublicMsg b of
                        Just c ->
                            case c of
                                Http.Server.Worker.RequestReceived d ->
                                    updateImageByMessage config (config.requestReceived d)

                        Nothing ->
                            Platform.Extra.noOperation
                    )

        ImageMessageReceived b ->
            updateImage config b model

        ExitRequested ->
            closeServer model
                |> Platform.Extra.andThen (updateImageByMessage config config.exitRequested)



--


updateServer : Http.Server.Worker.Msg -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
updateServer msg model =
    Http.Server.Worker.update msg model.server
        |> Tuple.mapBoth (\x -> { model | server = x }) (Cmd.map ServerMessageReceived)


closeServer : Model msg a -> ( Model msg a, Cmd (Msg a msg) )
closeServer model =
    Http.Server.Worker.close model.server
        |> Tuple.mapBoth (\x -> { model | server = x }) (Cmd.map ServerMessageReceived)



--


updateImage : Config msg a -> StateMachine.Worker.Msg a msg -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
updateImage config msg model =
    StateMachine.Worker.update config.stateMachineConfig msg model.state
        |> Tuple.mapBoth (\x -> { model | state = x }) (Cmd.map ImageMessageReceived)


updateImageByMessage : Config msg a -> msg -> Model msg a -> ( Model msg a, Cmd (Msg a msg) )
updateImageByMessage config msg model =
    StateMachine.Worker.updateByMessage config.stateMachineConfig msg model.state
        |> Tuple.mapBoth (\x -> { model | state = x }) (Cmd.map ImageMessageReceived)



--


subscriptions : Config msg a -> Model msg a -> Sub (Msg a msg)
subscriptions config model =
    Sub.batch
        [ Http.Server.Worker.subscriptions model.server
            |> Sub.map ServerMessageReceived
        , StateMachine.Worker.subscriptions config.stateMachineConfig model.state
            |> Sub.map ImageMessageReceived
        ]
