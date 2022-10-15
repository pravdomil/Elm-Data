module Main exposing (..)

import Codec
import FileSystem
import MemoryImage.FileImage
import MemoryImage.FileSystem
import MemoryImage.Worker
import Platform.Extra
import Process.Extra
import Time
import Time.Codec


main =
    MemoryImage.Worker.worker config config2 (FileSystem.Path "image.jsonl")


config : MemoryImage.FileSystem.Config Msg Model
config =
    MemoryImage.FileSystem.Config
        init
        update
        subscriptions
        (.status >> statusToDailySave)


config2 : MemoryImage.FileImage.Config Msg Model
config2 =
    MemoryImage.FileImage.Config
        (Codec.encoder codec)
        (Codec.decoder codec)
        (Codec.encoder msgCodec)
        (Codec.decoder msgCodec)



--


type alias Model =
    { messages : List String
    , status : Status
    }


init : Maybe Model -> ( Model, Cmd Msg )
init a =
    ( case a of
        Just b ->
            { b
                | status = Running
            }

        Nothing ->
            Model
                [ "Welcome." ]
                Running
    , Process.Extra.onExitSignal ExitSignalReceived
    )



--


type Status
    = Running
    | Exiting


statusToDailySave : Status -> MemoryImage.FileSystem.DailySave
statusToDailySave a =
    case a of
        Running ->
            MemoryImage.FileSystem.DailySave

        Exiting ->
            MemoryImage.FileSystem.NoDailySave



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
            ( { model | status = Exiting }
            , Cmd.none
            )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Running ->
            Time.every 1000 LogTime

        Exiting ->
            Sub.none



--


codec : Codec.Codec Model
codec =
    Codec.record (\x1 x2 -> { messages = x1, status = x2 })
        |> Codec.field .messages (Codec.list Codec.string)
        |> Codec.field .status statusCodec
        |> Codec.buildRecord


statusCodec : Codec.Codec Status
statusCodec =
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
