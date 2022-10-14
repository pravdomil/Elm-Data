module MemoryImage.FileImage exposing (Config, FileImage, create, fromString, image, toString)

import Json.Decode
import Json.Encode
import Result.Extra


{-| Memory image file format.

It is a JSONL format where first line is the image and the rest of the lines are messages.

-}
type FileImage msg a
    = FileImage (List msg) a


create : List msg -> a -> FileImage msg a
create =
    FileImage


fromString : Config msg a -> String -> Result Json.Decode.Error (FileImage msg a)
fromString config a =
    case String.split "\n" a of
        [] ->
            Err (Json.Decode.Failure "Cannot decode image." (Json.Encode.string a))

        first :: rest ->
            Result.map2
                FileImage
                (rest
                    |> List.map (Json.Decode.decodeString config.msgDecoder)
                    |> Result.Extra.sequence
                )
                (first
                    |> Json.Decode.decodeString config.decoder
                )


image : (msg -> a -> ( a, Cmd msg )) -> FileImage msg a -> a
image updateFn (FileImage messages a) =
    messages
        |> List.foldl (\msg x -> updateFn msg x |> Tuple.first) a


toString : Config msg a -> FileImage msg a -> String
toString config (FileImage messages a) =
    messages
        |> List.map (config.msgEncoder >> Json.Encode.encode 0)
        |> (::) (Json.Encode.encode 0 (config.encoder a))
        |> String.join "\n"



--


type alias Config msg a =
    { encoder : a -> Json.Decode.Value
    , decoder : Json.Decode.Decoder a

    --
    , msgEncoder : msg -> Json.Decode.Value
    , msgDecoder : Json.Decode.Decoder msg
    }
