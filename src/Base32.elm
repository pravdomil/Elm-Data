module Base32 exposing (..)

import Bitwise


convert30BitsTo6Chars : Int -> List Char
convert30BitsTo6Chars a =
    [ a |> Bitwise.shiftRightBy (5 * 0) |> convert5BitsToChar
    , a |> Bitwise.shiftRightBy (5 * 1) |> convert5BitsToChar
    , a |> Bitwise.shiftRightBy (5 * 2) |> convert5BitsToChar
    , a |> Bitwise.shiftRightBy (5 * 3) |> convert5BitsToChar
    , a |> Bitwise.shiftRightBy (5 * 4) |> convert5BitsToChar
    , a |> Bitwise.shiftRightBy (5 * 5) |> convert5BitsToChar
    ]


convert5BitsToChar : Int -> Char
convert5BitsToChar a =
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
