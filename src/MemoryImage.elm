module MemoryImage exposing (Config, MemoryImage, Operation(..), create, image, load, save, update)

import Json.Decode
import Json.Encode


type MemoryImage a
    = MemoryImage a


image : MemoryImage a -> a
image (MemoryImage a) =
    a



--


type alias Config msg a =
    { encode : a -> Json.Decode.Value
    , decoder : Json.Decode.Decoder a

    --
    , encodeMsg : msg -> Json.Decode.Value
    , msgDecoder : Json.Decode.Decoder msg
    }


create : Config msg a -> a -> ( MemoryImage a, Operation )
create config a =
    let
        image_ : MemoryImage a
        image_ =
            MemoryImage a
    in
    ( image_
    , save config image_
    )


update : Config msg a -> (msg -> a -> a) -> msg -> MemoryImage a -> ( MemoryImage a, Operation )
update config updateFn msg (MemoryImage a) =
    ( MemoryImage (updateFn msg a)
    , Append (msg |> config.encodeMsg |> Json.Encode.encode 0 |> (++) "\n")
    )



--


load : Config msg a -> (msg -> a -> a) -> String -> Result Json.Decode.Error (MemoryImage a)
load config updateFn a =
    decodeDiskImage config a |> Result.map (diskImageToMemoryImage updateFn)


save : Config msg a -> MemoryImage a -> Operation
save config (MemoryImage a) =
    Overwrite (a |> config.encode |> Json.Encode.encode 0)



--


type Operation
    = Overwrite String
    | Append String



--


type DiskImage msg a
    = DiskImage a (List msg)


diskImageToMemoryImage : (msg -> a -> a) -> DiskImage msg a -> MemoryImage a
diskImageToMemoryImage updateFn (DiskImage a messages) =
    messages
        |> List.foldl updateFn a
        |> MemoryImage


decodeDiskImage : Config msg a -> String -> Result Json.Decode.Error (DiskImage msg a)
decodeDiskImage config a =
    case String.split "\n" a of
        [] ->
            Err (Json.Decode.Failure "Cannot decode disk image." (Json.Encode.string a))

        first :: rest ->
            Result.map2 DiskImage
                (Json.Decode.decodeString config.decoder first)
                (rest
                    |> List.map (Json.Decode.decodeString config.msgDecoder)
                    |> resultSequence
                )


encodeDiskImage : Config msg a -> DiskImage msg a -> String
encodeDiskImage config (DiskImage a messages) =
    messages
        |> List.map (config.encodeMsg >> Json.Encode.encode 0)
        |> (::) (Json.Encode.encode 0 (config.encode a))
        |> String.join "\n"



--


resultSequence : List (Result x a) -> Result x (List a)
resultSequence a =
    List.foldr (Result.map2 (::)) (Ok []) a
