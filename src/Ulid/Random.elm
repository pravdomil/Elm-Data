module Ulid.Random exposing (..)

import JavaScript
import Json.Decode
import Json.Encode
import Task
import Time
import Ulid


generate : Task.Task x (Ulid.Ulid a)
generate =
    Task.map2 Ulid.fromTimeAndRandomness Time.now randomness


randomness : Task.Task x Ulid.Randomness
randomness =
    let
        decoder : Json.Decode.Decoder Ulid.Randomness
        decoder =
            Json.Decode.map3
                Ulid.Randomness
                (Json.Decode.index 0 Json.Decode.int)
                (Json.Decode.index 1 Json.Decode.int)
                (Json.Decode.index 2 Json.Decode.int)
    in
    JavaScript.run
        "Array.from((typeof crypto === 'undefined' ? require('crypto').webcrypto : crypto).getRandomValues(new Uint32Array(3)))"
        Json.Encode.null
        decoder
        |> Task.onError
            (\_ ->
                JavaScript.run
                    "[Math.random()*2**32, Math.random()*2**32, Math.random()*2**32]"
                    Json.Encode.null
                    decoder
            )
        |> Task.onError (\_ -> Task.succeed (Ulid.Randomness 0 0 0))
