module Image exposing (..)

import Codec


type alias Image =
    { counter : Int
    }


init : () -> Image
init () =
    Image 0



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
