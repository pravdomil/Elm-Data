module ID exposing (ID, fromString, toAny, toString)


type ID a
    = ID String


fromString : String -> ID a
fromString a =
    ID a



--


toString : ID a -> String
toString (ID a) =
    a


toAny : ID a -> ID b
toAny (ID a) =
    ID a
