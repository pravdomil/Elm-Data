module LogMessage exposing (..)

import Codec
import Console
import JavaScript
import JavaScript.Codec
import Json.Decode
import LogMessage
import Task


type alias LogMessage =
    { type_ : Type
    , author : String
    , message : String
    , detail : Maybe Detail
    }


log : LogMessage -> Task.Task JavaScript.Error ()
log a =
    let
        fn : String -> Task.Task JavaScript.Error ()
        fn =
            case a.type_ of
                Info ->
                    Console.logInfo

                Warning ->
                    Console.logWarning

                Error ->
                    Console.logError
    in
    fn (Codec.encodeToString 0 LogMessage.codec a)



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
    Codec.record (\x1 x2 x3 x4 -> { type_ = x1, author = x2, message = x3, detail = x4 })
        |> Codec.field .type_ typeCodec
        |> Codec.field .author Codec.string
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
