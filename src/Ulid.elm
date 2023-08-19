module Ulid exposing (Ulid, codec, fromTimeAndRandomness, toAny, toString)

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
type Ulid a
    = Ulid String


fromTimeAndRandomness : Time.Posix -> ( Int, Int, Int ) -> Ulid a
fromTimeAndRandomness time ( random1, random2, random3 ) =
    let
        encode20bits : Int -> String
        encode20bits b =
            String.padLeft 4 '0' (Result.withDefault "" (Crockford.encode (Bitwise.and 0x000FFFFF b)))

        encode30bits : Int -> String
        encode30bits b =
            String.padLeft 6 '0' (Result.withDefault "" (Crockford.encode (Bitwise.and 0x3FFFFFFF b)))
    in
    Ulid
        (encode30bits (Time.posixToMillis time // 0x00100000)
            ++ encode20bits (Time.posixToMillis time)
            --
            ++ encode30bits random1
            ++ encode30bits random2
            ++ encode20bits random3
        )


toString : Ulid a -> String
toString (Ulid a) =
    a


toAny : Ulid a -> Ulid b
toAny (Ulid a) =
    Ulid a


codec : Codec.Codec (Ulid a)
codec =
    Codec.map toString Ulid Codec.string
