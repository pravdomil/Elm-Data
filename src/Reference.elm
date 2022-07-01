module Reference exposing (Reference, codec, fromString, toAny, toId, toString)

import Codec
import Id


type Reference a
    = Reference String


fromString : String -> Reference a
fromString a =
    Reference a



--


toString : Reference a -> String
toString (Reference a) =
    a


toId : Reference a -> Id.Id a
toId (Reference a) =
    Id.fromString a


toAny : Reference a -> Reference b
toAny (Reference a) =
    Reference a



--


codec : Codec.Codec (Reference a)
codec =
    Codec.string |> Codec.map fromString toString
