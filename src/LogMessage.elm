module LogMessage exposing (..)

import Codec
import JavaScript
import JavaScript.Codec
import Json.Decode


type alias LogMessage =
    { type_ : Type
    , message : String
    , detail : Maybe Detail
    }



--


type Type
    = Info
    | Warning
    | Error



--


type Detail
    = DecodeError Json.Decode.Error
    | JavaScriptError JavaScript.Error



--


codec : Codec.Codec LogMessage
codec =
    Codec.record (\x1 x2 x3 -> { type_ = x1, message = x2, detail = x3 })
        |> Codec.field .type_ typeCodec
        |> Codec.field .message Codec.string
        |> Codec.field .detail (Codec.maybe detailCodec)
        |> Codec.buildRecord


detailCodec : Codec.Codec Detail
detailCodec =
    Codec.lazy
        (\() ->
            Codec.custom
                (\fn1 fn2 x ->
                    case x of
                        DecodeError x1 ->
                            fn1 x1

                        JavaScriptError x1 ->
                            fn2 x1
                )
                |> Codec.variant1 DecodeError JavaScript.Codec.jsonDecodeErrorCodec
                |> Codec.variant1 JavaScriptError JavaScript.Codec.errorCodec
                |> Codec.buildCustom
        )


typeCodec : Codec.Codec Type
typeCodec =
    Codec.lazy
        (\() ->
            Codec.custom
                (\fn1 fn2 fn3 x ->
                    case x of
                        Info ->
                            fn1

                        Warning ->
                            fn2

                        Error ->
                            fn3
                )
                |> Codec.variant0 Info
                |> Codec.variant0 Warning
                |> Codec.variant0 Error
                |> Codec.buildCustom
        )
