module LogMessage exposing (..)

import Codec
import Console
import JavaScript
import JavaScript.Codec
import Json.Decode
import Task


type LogMessage
    = Info (List String)
    | Warning (List String)
    | Error (List String)


log : LogMessage -> Task.Task JavaScript.Error ()
log a =
    case a of
        Info _ ->
            Console.logInfo (Codec.encodeToString codec a)

        Warning _ ->
            Console.logWarning (Codec.encodeToString codec a)

        Error _ ->
            Console.logError (Codec.encodeToString codec a)


jsonDecodeError : Type -> String -> List String -> Json.Decode.Error -> LogMessage
jsonDecodeError a b c d =
    LogMessage a b (c ++ [ Codec.encodeToString JavaScript.Codec.jsonDecodeErrorCodec d ])


javaScriptError : Type -> String -> List String -> JavaScript.Error -> LogMessage
javaScriptError a b c d =
    LogMessage a b (c ++ [ Codec.encodeToString JavaScript.Codec.errorCodec d ])



--


codec : Codec.Codec LogMessage
codec =
    Codec.custom
        (\fn1 fn2 fn3 x ->
            case x of
                Info x1 ->
                    fn1 x1

                Warning x1 ->
                    fn2 x1

                Error x1 ->
                    fn3 x1
        )
        |> Codec.variant1 Info (Codec.list Codec.string)
        |> Codec.variant1 Warning (Codec.list Codec.string)
        |> Codec.variant1 Error (Codec.list Codec.string)
        |> Codec.buildCustom
