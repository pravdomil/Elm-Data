module Id.Codec exposing (..)

import Codec
import Id


id : Codec.Codec (Id.Id a)
id =
    Codec.string |> Codec.map Id.fromString Id.toString
