module Ulid exposing (Randomness, Ulid, codec, fromTimeAndRandomness, toAny, toId, toString)

import Bitwise
import Codec
import Crockford
import Id
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


fromTimeAndRandomness : Time.Posix -> Randomness -> Ulid a
fromTimeAndRandomness time randomness =
    let
        encode18bits : Int -> String
        encode18bits b =
            String.padLeft 4 '0' (Result.withDefault "" (Crockford.encode (Bitwise.and 0x0003FFFF b)))

        encode20bits : Int -> String
        encode20bits b =
            String.padLeft 4 '0' (Result.withDefault "" (Crockford.encode (Bitwise.and 0x000FFFFF b)))

        encode30bits : Int -> String
        encode30bits b =
            String.padLeft 6 '0' (Result.withDefault "" (Crockford.encode (Bitwise.and 0x3FFFFFFF b)))
    in
    Ulid
        (encode18bits (Time.posixToMillis time // 0x40000000)
            ++ encode30bits (Time.posixToMillis time)
            --
            ++ encode30bits randomness.a
            ++ encode30bits randomness.b
            ++ encode20bits randomness.c
        )


toString : Ulid a -> String
toString (Ulid a) =
    a


toAny : Ulid a -> Ulid b
toAny (Ulid a) =
    Ulid a


toId : Ulid a -> Id.Id a
toId a =
    Id.fromString (toString a)


codec : Codec.Codec (Ulid a)
codec =
    Codec.map toString Ulid Codec.string



--


type alias Randomness =
    { a : Int
    , b : Int
    , c : Int
    }
