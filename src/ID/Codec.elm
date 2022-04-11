module ID.Codec exposing (..)

import Codec
import ID


id : Codec.Codec (ID.ID a)
id =
    Codec.string |> Codec.map ID.fromString ID.toString
