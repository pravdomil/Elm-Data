module Image exposing (..)

import Codec
import MemoryImage.FileImage


type alias Image =
    { counter : Int
    }


init : () -> Image
init () =
    Image 0


config : MemoryImage.FileImage.Config Msg Image
config =
    MemoryImage.FileImage.Config
        (Codec.encoder codec)
        (Codec.decoder codec)
        (Codec.encoder msgCodec)
        (Codec.decoder msgCodec)



--


type Msg
    = IncreaseCounter



--


codec : Codec.Codec Image
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
