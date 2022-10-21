module Main exposing (..)

import Codec
import Json.Decode
import MemoryImage.FileImage
import MemoryImage.Worker
import Platform.Extra
import Process.Extra
import RunningState
import Time


main =
    Platform.worker
        { init = MemoryImage.Worker.init config
        , update =
            \msg a ->
                MemoryImage.Worker.update config msg a
                    |> (\( x, cmd ) ->
                            let
                                _ =
                                    Debug.log "" ""

                                _ =
                                    Debug.log "Message" msg

                                _ =
                                    Debug.log "Image" (MemoryImage.Worker.image x)
                            in
                            ( x
                            , cmd
                            )
                       )
        , subscriptions = MemoryImage.Worker.subscriptions config
        }


config : MemoryImage.Worker.Config Msg Model
config =
    MemoryImage.Worker.defaultConfig
        (MemoryImage.FileImage.Config
            (Codec.encoder modelCodec)
            (Codec.decoder modelCodec)
            (Codec.encoder msgCodec)
            (Codec.decoder msgCodec)
        )
        init
        update
        subscriptions
        (\x ->
            FlagsReceived
                (x
                    |> Json.Decode.decodeValue (Json.Decode.at [ "global", "process", "pid" ] Json.Decode.int)
                    |> Result.withDefault 0
                )
        )



--


type alias Model =
    { messages : List String
    , state : RunningState.RunningState
    }


init : () -> Model
init _ =
    Model
        [ "Welcome." ]
        RunningState.Running



--


type Msg
    = NothingHappened
    | FlagsReceived Int
    | LogNumber Int
    | ExitRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        FlagsReceived b ->
            ( { model
                | messages = ("Running with PID " ++ String.fromInt b ++ ".") :: model.messages
              }
            , Process.Extra.onInterruptAndTerminationSignal ExitRequested
            )

        LogNumber b ->
            ( { model | messages = String.fromInt b :: model.messages }
            , Cmd.none
            )

        ExitRequested ->
            ( { model | state = RunningState.Exiting }
            , Cmd.none
            )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        RunningState.Running ->
            Time.every 1000 (\x -> LogNumber (x |> Time.posixToMillis |> (\x2 -> modBy 1000 (x2 // 1000))))

        RunningState.Exiting ->
            Sub.none



--


modelCodec : Codec.Codec Model
modelCodec =
    Codec.record (\x1 x2 -> { messages = x1, state = x2 })
        |> Codec.field .messages (Codec.list Codec.string)
        |> Codec.field .state RunningState.codec
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

                        LogNumber x1 ->
                            fn3 x1

                        ExitRequested ->
                            fn4
                )
                |> Codec.variant0 NothingHappened
                |> Codec.variant1 FlagsReceived Codec.int
                |> Codec.variant1 LogNumber Codec.int
                |> Codec.variant0 ExitRequested
                |> Codec.buildCustom
        )
