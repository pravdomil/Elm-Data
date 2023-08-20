module Main exposing (..)

import Codec
import Json.Decode
import Platform.Extra
import StateMachine.Lifecycle
import StateMachine.Worker
import Time


main =
    Platform.worker
        { init = StateMachine.Worker.init config
        , update =
            \msg a ->
                StateMachine.Worker.update config msg a
                    |> (\( x, cmd ) ->
                            let
                                _ =
                                    Debug.log "" ""

                                _ =
                                    Debug.log "Message" msg

                                _ =
                                    Debug.log "State" x
                            in
                            ( x
                            , cmd
                            )
                       )
        , subscriptions = StateMachine.Worker.subscriptions config
        }


config : StateMachine.Worker.Config Msg Model
config =
    StateMachine.Worker.Config
        init
        update
        subscriptions
        modelCodec
        msgCodec
        StateMachine.Worker.flagsToFilePath
        FlagsReceived
        .lifecycle
        LifecycleChanged



--


type alias Model =
    { messages : List String
    , lifecycle : StateMachine.Lifecycle.Lifecycle
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( Model
        [ "Welcome." ]
        StateMachine.Lifecycle.Running
    , Cmd.none
    )



--


type Msg
    = NothingHappened
    | FlagsReceived Json.Decode.Value
    | LifecycleChanged StateMachine.Lifecycle.Lifecycle
      --
    | LogNumber Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        FlagsReceived b ->
            let
                pid : Int
                pid =
                    Result.withDefault 0 (Json.Decode.decodeValue (Json.Decode.at [ "global", "process", "pid" ] Json.Decode.int) b)
            in
            ( { model
                | messages = ("Running with PID " ++ String.fromInt pid ++ ".") :: model.messages
              }
            , Cmd.none
            )

        LifecycleChanged b ->
            ( { model | lifecycle = b }
            , Cmd.none
            )

        LogNumber b ->
            ( { model | messages = String.fromInt b :: model.messages }
            , Cmd.none
            )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.lifecycle of
        StateMachine.Lifecycle.Running ->
            Time.every 1000 (\x -> LogNumber (Time.posixToMillis x |> (\x2 -> modBy 1000 (x2 // 1000))))

        StateMachine.Lifecycle.Exiting ->
            Sub.none



--


modelCodec : Codec.Codec Model
modelCodec =
    Codec.record (\x1 x2 -> { messages = x1, lifecycle = x2 })
        |> Codec.field .messages (Codec.list Codec.string)
        |> Codec.field .lifecycle StateMachine.Lifecycle.codec
        |> Codec.buildRecord


msgCodec : Codec.Codec Msg
msgCodec =
    Codec.lazy
        (\() ->
            Codec.custom
                (\fn1 fn2 fn3 fn4 x ->
                    case x of
                        NothingHappened ->
                            fn1

                        FlagsReceived x1 ->
                            fn2 x1

                        LifecycleChanged x1 ->
                            fn3 x1

                        LogNumber x1 ->
                            fn4 x1
                )
                |> Codec.variant0 NothingHappened
                |> Codec.variant1 FlagsReceived Codec.value
                |> Codec.variant1 LifecycleChanged StateMachine.Lifecycle.codec
                |> Codec.variant1 LogNumber Codec.int
                |> Codec.buildCustom
        )
