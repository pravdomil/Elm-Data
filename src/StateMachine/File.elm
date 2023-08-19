module StateMachine.File exposing (File, create, fromString, state, toString)

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
                (Result.Extra.sequence (List.map (Codec.decodeString codecMsg) rest))
                (Codec.decodeString codecA first)


state : (msg -> a -> ( a, Cmd msg )) -> File msg a -> a
state updateFn (File messages a) =
    List.foldl (\msg x -> Tuple.first (updateFn msg x)) a messages


toString : Codec.Codec a -> Codec.Codec msg -> File msg a -> String
toString codecA codecMsg (File messages a) =
    String.join "\n"
        (Codec.encodeToString 0 codecA a
            :: List.map (Codec.encodeToString 0 codecMsg) messages
        )
