module Main exposing (..)

import Codec
import FileSystem
import Json.Decode
import MemoryImage.FileImage
import MemoryImage.Worker
import Platform.Extra
import Process.Extra
import Time
import Time.Codec


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
    MemoryImage.Worker.Config
        (MemoryImage.FileImage.Config
            (Codec.encoder modelCodec)
            (Codec.decoder modelCodec)
            (Codec.encoder msgCodec)
            (Codec.decoder msgCodec)
        )
        init
        update
        subscriptions
        (\_ -> FileSystem.Path "image.jsonl")
        (.state >> stateToDailySave)



--


type alias Model =
    { messages : List String
    , state : State
    }


init : Json.Decode.Value -> Maybe Model -> ( Model, Cmd Msg )
init flags a =
    let
        model : Model
        model =
            case a of
                Just b ->
                    b

                Nothing ->
                    Model
                        [ "Welcome." ]
                        Running

        pid : Int
        pid =
            flags
                |> Json.Decode.decodeValue (Json.Decode.at [ "global", "process", "pid" ] Json.Decode.int)
                |> Result.withDefault 0
    in
    ( model
        |> (\x ->
                { x
                    | messages = ("Running with pid " ++ String.fromInt pid ++ ".") :: x.messages
                    , state = Running
                }
           )
    , Process.Extra.onExitSignal ExitSignalReceived
    )



--


type State
    = Running
    | Exiting


stateToDailySave : State -> MemoryImage.Worker.DailySave
stateToDailySave a =
    case a of
        Running ->
            MemoryImage.Worker.DailySave

        Exiting ->
            MemoryImage.Worker.NoDailySave



--


type Msg
    = NothingHappened
    | LogTime Time.Posix
    | ExitSignalReceived


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        LogTime b ->
            let
                message : String
                message =
                    String.fromInt (b |> Time.posixToMillis |> (\x -> modBy 1000 (x // 1000)))
            in
            ( { model | messages = message :: model.messages }
            , Cmd.none
            )

        ExitSignalReceived ->
            ( { model | state = Exiting }
            , Cmd.none
            )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running ->
            Time.every 1000 LogTime

        Exiting ->
            Sub.none



--


modelCodec : Codec.Codec Model
modelCodec =
    Codec.record (\x1 x2 -> { messages = x1, state = x2 })
        |> Codec.field .messages (Codec.list Codec.string)
        |> Codec.field .state stateCodec
        |> Codec.buildRecord


stateCodec : Codec.Codec State
stateCodec =
    Codec.lazy
        (\() ->
            Codec.custom
                (\fn1 fn2 x ->
                    case x of
                        Running ->
                            fn1

                        Exiting ->
                            fn2
                )
                |> Codec.variant0 Running
                |> Codec.variant0 Exiting
                |> Codec.buildCustom
        )


msgCodec : Codec.Codec Msg
msgCodec =
    Codec.lazy
        (\() ->
            Codec.custom
                (\fn1 fn2 fn3 x ->
                    case x of
                        NothingHappened ->
                            fn1

                        LogTime x1 ->
                            fn2 x1

                        ExitSignalReceived ->
                            fn3
                )
                |> Codec.variant0 NothingHappened
                |> Codec.variant1 LogTime Time.Codec.posix
                |> Codec.variant0 ExitSignalReceived
                |> Codec.buildCustom
        )
