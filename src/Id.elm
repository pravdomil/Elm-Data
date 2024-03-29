module Id exposing (Id, codec, fromString, toAny, toString)

import Codec


type Id a
    = Id String


fromString : String -> Id a
fromString a =
    Id a


toString : Id a -> String
toString (Id a) =
    a


toAny : Id a -> Id b
toAny (Id a) =
    Id a


codec : Codec.Codec (Id a)
codec =
    Codec.map toString fromString Codec.string
