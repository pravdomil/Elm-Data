module MemoryImage.Server exposing (Config, defaultConfig, init, subscriptions, update, worker)

import Http.Server
import Http.Server.Worker
import Json.Decode
import MemoryImage.FileImage
import MemoryImage.Worker
import Platform.Extra
import Process.Extra
import RunningState


worker : Config msg a -> Program Json.Decode.Value (Model msg a) (Msg msg)
worker config =
    Platform.worker
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
        }



--


type alias Config msg a =
    { imageConfig : MemoryImage.Worker.Config msg a
    , flagsToServerOptions : Json.Decode.Value -> Http.Server.Options

    --
    , requestReceived : Http.Server.Request -> msg
    , exitSignalReceived : msg
    }


defaultConfig :
    MemoryImage.FileImage.Config msg { a | state : RunningState.RunningState }
    -> (Json.Decode.Value -> Maybe { a | state : RunningState.RunningState } -> ( { a | state : RunningState.RunningState }, Cmd msg ))
    -> (msg -> { a | state : RunningState.RunningState } -> ( { a | state : RunningState.RunningState }, Cmd msg ))
    -> ({ a | state : RunningState.RunningState } -> Sub msg)
    -> (Http.Server.Request -> msg)
    -> msg
    -> Config msg { a | state : RunningState.RunningState }
defaultConfig config init_ update_ subscriptions_ requestReceived exitSignalReceived =
    Config
        (MemoryImage.Worker.defaultConfig config init_ update_ subscriptions_)
        (\x ->
            x
                |> Json.Decode.decodeValue (Json.Decode.at [ "global", "process", "env", "serverOptions" ] Json.Decode.string)
                |> Result.andThen (Json.Decode.decodeString Http.Server.optionsDecoder)
                |> Result.withDefault Http.Server.emptyOptions
        )
        requestReceived
        exitSignalReceived



--


type alias Model msg a =
    { server : Http.Server.Worker.Worker
    , image : MemoryImage.Worker.Image msg a
    }


init : Config msg a -> Json.Decode.Value -> ( Model msg a, Cmd (Msg msg) )
init config flags =
    let
        ( server, cmd ) =
            Http.Server.Worker.init (config.flagsToServerOptions flags)

        ( image, cmd2 ) =
            MemoryImage.Worker.init config.imageConfig flags
    in
    ( Model server image
    , Cmd.batch
        [ cmd |> Cmd.map ServerMessageReceived
        , cmd2 |> Cmd.map ImageMessageReceived
        , Process.Extra.onExitSignal ExitSignalReceived
        ]
    )



--


type Msg msg
    = ServerMessageReceived Http.Server.Worker.Msg
    | ImageMessageReceived (MemoryImage.Worker.Msg msg)
    | ExitSignalReceived


update : Config msg a -> Msg msg -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
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

        ExitSignalReceived ->
            closeServer model
                |> Platform.Extra.andThen (updateImageByMessage config config.exitSignalReceived)



--


updateServer : Http.Server.Worker.Msg -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
updateServer msg model =
    Http.Server.Worker.update msg model.server
        |> Tuple.mapBoth (\x -> { model | server = x }) (Cmd.map ServerMessageReceived)


closeServer : Model msg a -> ( Model msg a, Cmd (Msg msg) )
closeServer model =
    Http.Server.Worker.close model.server
        |> Tuple.mapBoth (\x -> { model | server = x }) (Cmd.map ServerMessageReceived)



--


updateImage : Config msg a -> MemoryImage.Worker.Msg msg -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
updateImage config msg model =
    MemoryImage.Worker.update config.imageConfig msg model.image
        |> Tuple.mapBoth (\x -> { model | image = x }) (Cmd.map ImageMessageReceived)


updateImageByMessage : Config msg a -> msg -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
updateImageByMessage config msg model =
    MemoryImage.Worker.updateByMessage config.imageConfig msg model.image
        |> Tuple.mapBoth (\x -> { model | image = x }) (Cmd.map ImageMessageReceived)



--


subscriptions : Config msg a -> Model msg a -> Sub (Msg msg)
subscriptions config model =
    Sub.batch
        [ Http.Server.Worker.subscriptions model.server
            |> Sub.map ServerMessageReceived
        , MemoryImage.Worker.subscriptions config.imageConfig model.image
            |> Sub.map ImageMessageReceived
        ]
