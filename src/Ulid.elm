module Ulid exposing (Ulid, codec, toString)

import Bitwise
import Codec
import Crockford
import Time


{-| ULID is composed of a timestamp and a randomness.

     01AN4Z07BY      79KA1307SR9X4MV3

    |----------|    |----------------|
     Timestamp       Randomness
     48 bits         80 bits
     10 characters   16 characters

<https://github.com/ulid/spec>

-}
type Ulid
    = Ulid String


fromTimeAndRandomness : Time.Posix -> Int -> Int -> Ulid
fromTimeAndRandomness timestamp random1 random2 =
    Ulid
        (String.padLeft 10 '0' (Result.withDefault "" (Crockford.encode (Bitwise.and 0x0000FFFFFFFFFFFF (Time.posixToMillis timestamp))))
            ++ String.padLeft 8 '0' (Result.withDefault "" (Crockford.encode (Bitwise.and 0x000000FFFFFFFFFF random1)))
            ++ String.padLeft 8 '0' (Result.withDefault "" (Crockford.encode (Bitwise.and 0x000000FFFFFFFFFF random2)))
        )


toString : Ulid -> String
toString (Ulid a) =
    a


codec : Codec.Codec Ulid
codec =
    Codec.custom
        (\fn1 x ->
            case x of
                Ulid x1 ->
                    fn1 x1
        )
        |> Codec.variant1 Ulid Codec.string
        |> Codec.buildCustom
