module ID exposing (ID, fromInt, fromString, toAny, toString)


type ID a
    = ID String


fromString : String -> Maybe (ID a)
fromString a =
    let
        isAllowedChar : Char -> Bool
        isAllowedChar b =
            Char.isAlphaNum b || b == '_' || b == '-'
    in
    if
        (String.length a >= 1)
            && (String.length a <= 128)
            && String.all isAllowedChar a
    then
        Just (ID a)

    else
        Nothing


fromInt : Int -> ID a
fromInt a =
    String.fromInt a |> ID



--


toString : ID a -> String
toString (ID a) =
    a


toAny : ID a -> ID b
toAny (ID a) =
    ID a
