module LogMessage exposing (..)

import Codec
import Console
import Id
import Id.Random
import JavaScript
import JavaScript.Codec
import Task
import Time
import Time.Codec


type alias LogMessage =
    { id : Id.Id ()
    , time : Time.Posix
    , related : List (Id.Id ())
    , type_ : Type
    }


codec : Codec.Codec LogMessage
codec =
    Codec.object LogMessage
        |> Codec.field "id" .id Id.codec
        |> Codec.field "time" .time Time.Codec.posix
        |> Codec.field "related" .related (Codec.list Id.codec)
        |> Codec.field "type_" .type_ typeCodec
        |> Codec.buildObject


type Type
    = Info String
    | Warning String
    | Error String JavaScript.Error


typeCodec : Codec.Codec Type
typeCodec =
    Codec.custom
        (\fn1 fn2 fn3 v ->
            case v of
                Info v1 ->
                    fn1 v1

                Warning v1 ->
                    fn2 v1

                Error v1 v2 ->
                    fn3 v1 v2
        )
        |> Codec.variant1 "Info" Info Codec.string
        |> Codec.variant1 "Warning" Warning Codec.string
        |> Codec.variant2 "Error" Error Codec.string JavaScript.Codec.errorCodec
        |> Codec.buildCustom



--


log : List (Id.Id ()) -> Type -> Task.Task x ()
log ids type_ =
    let
        message_ : Task.Task JavaScript.Error LogMessage
        message_ =
            Task.map2
                (\v v2 ->
                    { id = v
                    , time = v2
                    , related = ids
                    , type_ = type_
                    }
                )
                (Id.Random.generate
                    |> Task.onError (\_ -> Task.succeed (Id.fromString ""))
                )
                Time.now
    in
    message_
        |> Task.andThen
            (\v ->
                let
                    data : String
                    data =
                        v |> Codec.encodeToString 0 codec
                in
                case v.type_ of
                    Info _ ->
                        Console.log data

                    Warning _ ->
                        Console.log data

                    Error _ _ ->
                        Console.logError data
            )
        |> Task.map (\_ -> ())
        |> Task.onError (\_ -> Task.succeed ())
