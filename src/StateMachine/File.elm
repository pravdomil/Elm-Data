module StateMachine.File exposing (File, create, fromString, image, toString)

import Codec
import Json.Decode
import Json.Encode
import Result.Extra


{-| Representation of state machine JSONL file.

  - first line is the initial state
  - the rest of the lines are messages.

-}
type File msg a
    = File (List msg) a


create : List msg -> a -> File msg a
create =
    File


fromString : Codec.Codec a -> Codec.Codec msg -> String -> Result Json.Decode.Error (File msg a)
fromString codecA codecMsg a =
    case String.split "\n" a of
        [] ->
            Err (Json.Decode.Failure "Cannot decode image." (Json.Encode.string a))

        first :: rest ->
            Result.map2
                File
                (rest
                    |> List.map (Codec.decodeString codecMsg)
                    |> Result.Extra.sequence
                )
                (first
                    |> Codec.decodeString codecA
                )


image : (msg -> a -> ( a, Cmd msg )) -> File msg a -> a
image updateFn (File messages a) =
    List.foldl (\msg x -> updateFn msg x |> Tuple.first) a messages


toString : Codec.Codec a -> Codec.Codec msg -> File msg a -> String
toString codecA codecMsg (File messages a) =
    String.join "\n"
        (Codec.encodeToString 0 codecA a
            :: List.map (Codec.encodeToString 0 codecMsg) messages
        )
