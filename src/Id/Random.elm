module Id.Random exposing (Error(..), generate)

{-| Generate 125-bit random ID.

ID is 25 characters long and is encoded using 5-bit characters.

Algorithm is inspired by [universally unique identifier (UUID)][uuid].

[uuid]: https://en.wikipedia.org/wiki/Universally_unique_identifier

-}

import Bitwise
import Id
import JavaScript
import Json.Decode
import Json.Encode
import Task


type Error
    = JavaScriptError JavaScript.Error


generate : Task.Task Error (Id.Id a)
generate =
    JavaScript.run
        "(function() { var a = new Int32Array(5); crypto.getRandomValues(a); return a })()"
        Json.Encode.null
        (Json.Decode.map5
            fromIntegers
            (Json.Decode.index 0 Json.Decode.int)
            (Json.Decode.index 1 Json.Decode.int)
            (Json.Decode.index 2 Json.Decode.int)
            (Json.Decode.index 3 Json.Decode.int)
            (Json.Decode.index 4 Json.Decode.int)
        )
        |> Task.mapError JavaScriptError


fromIntegers : Int -> Int -> Int -> Int -> Int -> Id.Id a
fromIntegers a1 a2 a3 a4 a5 =
    [ convert30BitsTo6Chars a1
    , convert30BitsTo6Chars a2
    , convert30BitsTo6Chars a3
    , convert30BitsTo6Chars a4
    , [ convert5BitsTo1Char a5 ]
    ]
        |> List.concat
        |> String.fromList
        |> Id.fromString


convert30BitsTo6Chars : Int -> List Char
convert30BitsTo6Chars a =
    [ a |> Bitwise.shiftRightBy (5 * 0) |> convert5BitsTo1Char
    , a |> Bitwise.shiftRightBy (5 * 1) |> convert5BitsTo1Char
    , a |> Bitwise.shiftRightBy (5 * 2) |> convert5BitsTo1Char
    , a |> Bitwise.shiftRightBy (5 * 3) |> convert5BitsTo1Char
    , a |> Bitwise.shiftRightBy (5 * 4) |> convert5BitsTo1Char
    , a |> Bitwise.shiftRightBy (5 * 5) |> convert5BitsTo1Char
    ]


convert5BitsTo1Char : Int -> Char
convert5BitsTo1Char a =
    case a |> Bitwise.and 31 of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            '_'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'f'

        15 ->
            'g'

        16 ->
            'h'

        17 ->
            'j'

        18 ->
            'k'

        19 ->
            'l'

        20 ->
            'm'

        21 ->
            'n'

        22 ->
            'p'

        23 ->
            'q'

        24 ->
            'r'

        25 ->
            's'

        26 ->
            't'

        27 ->
            'v'

        28 ->
            'w'

        29 ->
            'x'

        30 ->
            'y'

        _ ->
            'z'
