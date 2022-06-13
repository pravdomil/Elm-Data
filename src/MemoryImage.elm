module MemoryImage exposing (Config, DiskImage, LogMessage(..), MemoryImage, SaveImage(..), create, decodeDiskImage, diskImageToMemoryImage, encodeDiskImage, image, load, save, update)

import Json.Decode
import Json.Encode


type MemoryImage msg a
    = MemoryImage a


image : MemoryImage msg a -> a
image (MemoryImage a) =
    a



--


type alias Config msg a =
    { encoder : a -> Json.Decode.Value
    , decoder : Json.Decode.Decoder a

    --
    , msgEncoder : msg -> Json.Decode.Value
    , msgDecoder : Json.Decode.Decoder msg
    }


create : (() -> a) -> ( MemoryImage msg a, SaveImage a )
create initFn =
    let
        image_ : MemoryImage msg a
        image_ =
            initFn () |> MemoryImage
    in
    ( image_
    , save image_
    )


load : Config msg a -> (msg -> a -> a) -> String -> Result Json.Decode.Error (MemoryImage msg a)
load config updateFn a =
    decodeDiskImage config a |> Result.map (diskImageToMemoryImage updateFn)



--


update : (msg -> a -> a) -> msg -> MemoryImage msg a -> ( MemoryImage msg a, LogMessage msg )
update updateFn msg (MemoryImage a) =
    ( updateFn msg a |> MemoryImage
    , msg |> LogMessage
    )


save : MemoryImage msg a -> SaveImage a
save (MemoryImage a) =
    SaveImage a



--


type SaveImage a
    = SaveImage a



--


type LogMessage msg
    = LogMessage msg



--


type DiskImage msg a
    = DiskImage a (List msg)


diskImageToMemoryImage : (msg -> a -> a) -> DiskImage msg a -> MemoryImage msg a
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
        |> List.map (config.msgEncoder >> Json.Encode.encode 0)
        |> (::) (Json.Encode.encode 0 (config.encoder a))
        |> String.join "\n"



--


resultSequence : List (Result x a) -> Result x (List a)
resultSequence a =
    List.foldr (Result.map2 (::)) (Ok []) a
