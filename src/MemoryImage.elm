module MemoryImage exposing (Config, DiskImage, MemoryImage, decodeDiskImage, diskImageToMemoryImage, encodeDiskImage, image, init, load, update)

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


init : (() -> ( a, Cmd msg )) -> ( MemoryImage msg a, Cmd msg )
init initFn =
    initFn () |> Tuple.mapFirst MemoryImage


load : Config msg a -> (msg -> a -> ( a, Cmd msg )) -> String -> Result Json.Decode.Error (MemoryImage msg a)
load config updateFn a =
    decodeDiskImage config a |> Result.map (diskImageToMemoryImage updateFn)


update : (msg -> a -> ( a, Cmd msg )) -> msg -> MemoryImage msg a -> ( MemoryImage msg a, Cmd msg )
update updateFn msg (MemoryImage a) =
    updateFn msg a |> Tuple.mapFirst MemoryImage



--


type DiskImage msg a
    = DiskImage a (List msg)


diskImageToMemoryImage : (msg -> a -> ( a, Cmd msg )) -> DiskImage msg a -> MemoryImage msg a
diskImageToMemoryImage updateFn (DiskImage a messages) =
    messages
        |> List.foldl (\v1 v2 -> updateFn v1 v2 |> Tuple.first) a
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
