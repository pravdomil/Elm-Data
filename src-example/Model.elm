module Model exposing (..)

import Codec
import MemoryImage.FileImage
import MemoryImage.FileSystem
import Platform.Extra
import Time
import Time.Codec


config : MemoryImage.FileSystem.Config Msg Model
config =
    MemoryImage.FileSystem.Config
        init
        update


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
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model [ "Welcome." ]
    , Cmd.none
    )



--


type Msg
    = NothingHappened
    | LogTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        LogTime b ->
            let
                message : String
                message =
                    String.fromInt (Time.toSecond Time.utc b)
            in
            ( { model | messages = message :: model.messages }
            , Cmd.none
            )



--


codec : Codec.Codec Model
codec =
    Codec.record (\x1 -> { messages = x1 })
        |> Codec.field .messages (Codec.list Codec.string)
        |> Codec.buildRecord


msgCodec : Codec.Codec Msg
msgCodec =
    Codec.lazy
        (\() ->
            Codec.custom
                (\fn1 fn2 x ->
                    case x of
                        NothingHappened ->
                            fn1

                        LogTime x1 ->
                            fn2 x1
                )
                |> Codec.variant0 NothingHappened
                |> Codec.variant1 LogTime Time.Codec.posix
                |> Codec.buildCustom
        )
