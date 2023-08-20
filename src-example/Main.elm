module Main exposing (..)

import Codec
import Json.Decode
import Platform.Extra
import StateMachine.Lifecycle
import StateMachine.Worker
import Time


main =
    StateMachine.Worker.worker config


config : StateMachine.Worker.Config Msg Model
config =
    StateMachine.Worker.Config
        init
        update
        subscriptions
        modelCodec
        msgCodec
        StateMachine.Worker.flagsToFilePath
        (\x -> FlagsReceived (Result.withDefault 0 (Json.Decode.decodeValue (Json.Decode.at [ "global", "process", "pid" ] Json.Decode.int) x)))
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
    | FlagsReceived Int
    | LifecycleChanged StateMachine.Lifecycle.Lifecycle
      --
    | LogNumber Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        FlagsReceived b ->
            ( { model
                | messages = ("Running with PID " ++ String.fromInt b ++ ".") :: model.messages
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
    )
        |> (\( x, x2 ) ->
                let
                    _ =
                        Debug.log "" ""

                    _ =
                        Debug.log "Message" msg

                    _ =
                        Debug.log "Model" x
                in
                ( x, x2 )
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
                |> Codec.variant1 FlagsReceived Codec.int
                |> Codec.variant1 LifecycleChanged StateMachine.Lifecycle.codec
                |> Codec.variant1 LogNumber Codec.int
                |> Codec.buildCustom
        )
