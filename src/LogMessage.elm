module LogMessage exposing (..)

import Codec
import Console
import JavaScript
import Task


type alias LogMessage =
    { type_ : Type
    , author : String
    , messages : List String
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
    fn (Codec.encodeToString codec a)



--


type Type
    = Info
    | Warning
    | Error



--


codec : Codec.Codec LogMessage
codec =
    Codec.record (\x1 x2 x3 -> { type_ = x1, author = x2, messages = x3 })
        |> Codec.field .type_ typeCodec
        |> Codec.field .author Codec.string
        |> Codec.field .messages (Codec.list Codec.string)
        |> Codec.buildRecord


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
