module Model exposing (..)

import Codec
import MemoryImage.FileImage
import MemoryImage.FileSystem


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
    { counter : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model 0
    , Cmd.none
    )



--


type Msg
    = IncreaseCounter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncreaseCounter ->
            ( { model | counter = model.counter + 1 }
            , Cmd.none
            )



--


codec : Codec.Codec Model
codec =
    Codec.record (\x1 -> { counter = x1 })
        |> Codec.field .counter Codec.int
        |> Codec.buildRecord


msgCodec : Codec.Codec Msg
msgCodec =
    Codec.lazy
        (\() ->
            Codec.custom
                (\fn1 x ->
                    case x of
                        IncreaseCounter ->
                            fn1
                )
                |> Codec.variant0 IncreaseCounter
                |> Codec.buildCustom
        )
