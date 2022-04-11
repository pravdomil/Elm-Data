module ID.Codec exposing (..)

import Codec
import ID


id : Codec.Codec (ID.ID a)
id =
    Codec.string
        |> Codec.andThen
            (\v ->
                case ID.fromString v of
                    Just b ->
                        Codec.succeed b

                    Nothing ->
                        Codec.fail "Cannot decode ID."
            )
            ID.toString
