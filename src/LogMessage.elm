module LogMessage exposing (..)

import Codec
import Console
import Id
import Id.Random
import JavaScript
import JavaScript.Codec
import Json.Decode
import Task
import Time
import Time.Codec


type alias LogMessage =
    { id : Id.Id ()
    , time : Time.Posix

    --
    , type_ : Type
    , name : Name
    , details : Details
    , related : List (Id.Id ())
    }


codec : Codec.Codec LogMessage
codec =
    Codec.object LogMessage
        |> Codec.field "id" .id Id.codec
        |> Codec.field "time" .time Time.Codec.posix
        |> Codec.field "type_" .type_ typeCodec
        |> Codec.field "name" .name nameCodec
        |> Codec.field "details" .details detailsCodec
        |> Codec.field "related" .related (Codec.list Id.codec)
        |> Codec.buildObject



--


type Name
    = Name String


nameCodec : Codec.Codec Name
nameCodec =
    Codec.string |> Codec.map Name (\(Name v) -> v)



--


type Details
    = Details String
    | JsonDecodeError Json.Decode.Error
    | JavaScriptError JavaScript.Error


detailsCodec : Codec.Codec Details
detailsCodec =
    Codec.custom
        (\fn1 fn2 fn3 v ->
            case v of
                Details v1 ->
                    fn1 v1

                JsonDecodeError v1 ->
                    fn2 v1

                JavaScriptError v1 ->
                    fn3 v1
        )
        |> Codec.variant1 "Details" Details Codec.string
        |> Codec.variant1 "JsonDecodeError" JsonDecodeError JavaScript.Codec.jsonDecodeErrorCodec
        |> Codec.variant1 "JavaScriptError" JavaScriptError JavaScript.Codec.errorCodec
        |> Codec.buildCustom



--


type Type
    = Info
    | Warning
    | Error


typeCodec : Codec.Codec Type
typeCodec =
    Codec.custom
        (\fn1 fn2 fn3 v ->
            case v of
                Info ->
                    fn1

                Warning ->
                    fn2

                Error ->
                    fn3
        )
        |> Codec.variant0 "Info" Info
        |> Codec.variant0 "Warning" Warning
        |> Codec.variant0 "Error" Error
        |> Codec.buildCustom



--


log : Type -> Name -> Details -> List (Id.Id ()) -> Task.Task x ()
log type_ name details related =
    let
        message_ : Task.Task x LogMessage
        message_ =
            Task.map2
                (\v v2 ->
                    LogMessage v v2 type_ name details related
                )
                Id.Random.generate
                Time.now
    in
    message_
        |> Task.andThen
            (\v ->
                let
                    data : String
                    data =
                        Codec.encodeToString 0 codec v ++ "\n"
                in
                (case v.type_ of
                    Info ->
                        Console.log data

                    Warning ->
                        Console.log data

                    Error ->
                        Console.logError data
                )
                    |> Task.map (\_ -> ())
                    |> Task.onError (\_ -> Task.succeed ())
            )
