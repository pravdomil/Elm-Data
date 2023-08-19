module StateMachine.File exposing (FileImage, create, fromString, image, toString)

import Codec
import Json.Decode
import Json.Encode
import Result.Extra


{-| State machine JSONL file.

  - first line is the initial state
  - the rest of the lines are messages.

-}
type FileImage msg a
    = FileImage (List msg) a


create : List msg -> a -> FileImage msg a
create =
    FileImage


fromString : Codec.Codec a -> Codec.Codec msg -> String -> Result Json.Decode.Error (FileImage msg a)
fromString codecA codecMsg a =
    case String.split "\n" a of
        [] ->
            Err (Json.Decode.Failure "Cannot decode image." (Json.Encode.string a))

        first :: rest ->
            Result.map2
                FileImage
                (rest
                    |> List.map (Codec.decodeString codecMsg)
                    |> Result.Extra.sequence
                )
                (first
                    |> Codec.decodeString codecA
                )


image : (msg -> a -> ( a, Cmd msg )) -> FileImage msg a -> a
image updateFn (FileImage messages a) =
    messages
        |> List.foldl (\msg x -> updateFn msg x |> Tuple.first) a


toString : Codec.Codec a -> Codec.Codec msg -> FileImage msg a -> String
toString codecA codecMsg (FileImage messages a) =
    String.join "\n"
        (Codec.encodeToString 0 codecA a
            :: List.map (Codec.encodeToString 0 codecMsg) messages
        )
