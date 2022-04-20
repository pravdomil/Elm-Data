module Id exposing (Id, fromString, toAny, toString)


type Id a
    = Id String


fromString : String -> Id a
fromString a =
    Id a



--


toString : Id a -> String
toString (Id a) =
    a



--


toAny : Id a -> Id b
toAny (Id a) =
    Id a
