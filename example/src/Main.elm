module Main exposing (..)

import Codec
import Console
import FileSystem
import JavaScript
import MemoryImage
import MemoryImage.FileSystem
import Process.Extra
import Task
import Time


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



--


type alias Model =
    { status : Status
    , image : Maybe (MemoryImage.FileSystem.Image ImageMsg Image)
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Model Starting Nothing
    , Cmd.batch
        [ MemoryImage.FileSystem.open imageConfig initImage updateImage (FileSystem.Path "image.jsonl")
            |> Task.attempt GotImage
        , Process.Extra.onExit ProcessExit
        ]
    )



--


type Msg
    = GotImage (Result JavaScript.Error (MemoryImage.FileSystem.Image ImageMsg Image))
    | GotImageMsg ImageMsg
    | GotMemoryImageMsg MemoryImage.FileSystem.Msg
    | ProcessExit
    | NoOperation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Image" (model.image |> Maybe.map MemoryImage.FileSystem.image)
    in
    case msg of
        GotImage b ->
            case b of
                Ok c ->
                    ( { model | status = Running, image = Just c }
                    , Cmd.none
                    )

                Err c ->
                    ( model
                    , Console.logError (JavaScript.errorToString c)
                        |> Task.attempt (\_ -> NoOperation)
                    )

        GotImageMsg b ->
            case model.image of
                Just c ->
                    let
                        ( image, cmd ) =
                            MemoryImage.FileSystem.update updateImage b c
                                |> Tuple.mapBoth Just (Cmd.map GotMemoryImageMsg)

                        _ =
                            Debug.log "Image" (Maybe.map MemoryImage.FileSystem.image image)
                    in
                    ( { model | image = image }
                    , cmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        GotMemoryImageMsg b ->
            let
                ( image, cmd ) =
                    case model.image of
                        Just c ->
                            MemoryImage.FileSystem.updateMsg imageConfig b c
                                |> Tuple.mapBoth Just (Cmd.map GotMemoryImageMsg)

                        Nothing ->
                            ( Nothing
                            , Cmd.none
                            )
            in
            ( { model | image = image }
            , cmd
            )

        ProcessExit ->
            ( { model | status = Exiting }
            , MemoryImage.FileSystem.close |> Cmd.map GotMemoryImageMsg
            )

        NoOperation ->
            ( model
            , Cmd.none
            )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.status of
            Starting ->
                Sub.none

            Running ->
                Time.every 1000 (\_ -> GotImageMsg IncreaseCounter)

            Exiting ->
                Sub.none
        , case model.image of
            Just b ->
                MemoryImage.FileSystem.subscriptions b |> Sub.map GotMemoryImageMsg

            Nothing ->
                Sub.none
        ]



--


type Status
    = Starting
    | Running
    | Exiting



--


type alias Image =
    { counter : Int
    }


initImage : () -> Image
initImage () =
    Image 0


imageCodec : Codec.Codec Image
imageCodec =
    Codec.object Image
        |> Codec.field "counter" .counter Codec.int
        |> Codec.buildObject


imageConfig : MemoryImage.Config ImageMsg Image
imageConfig =
    MemoryImage.Config
        (Codec.encoder imageCodec)
        (Codec.decoder imageCodec)
        (Codec.encoder imageMsgCodec)
        (Codec.decoder imageMsgCodec)



--


type ImageMsg
    = IncreaseCounter


imageMsgCodec : Codec.Codec ImageMsg
imageMsgCodec =
    Codec.custom
        (\fn1 v ->
            case v of
                IncreaseCounter ->
                    fn1
        )
        |> Codec.variant0 "IncreaseCounter" IncreaseCounter
        |> Codec.buildCustom


updateImage : ImageMsg -> Image -> Image
updateImage msg model =
    case msg of
        IncreaseCounter ->
            { model | counter = model.counter + 1 }
