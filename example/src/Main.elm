module Main exposing (..)

import Codec
import FileSystem
import MemoryImage
import MemoryImage.FileSystem
import Process.Extra
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
    , image : MemoryImage.FileSystem.Image ImageMsg Image
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( image, cmd ) =
            MemoryImage.FileSystem.init imageConfig (FileSystem.Path "image.jsonl")
    in
    ( Model Running image
    , Cmd.batch
        [ cmd |> Cmd.map GotMemoryImageMsg
        , Process.Extra.onExitSignal ExitSignal
        ]
    )



--


type Msg
    = GotMemoryImageMsg (MemoryImage.FileSystem.Msg ImageMsg)
    | ExitSignal
    | Tick
    | NoOperation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        GotMemoryImageMsg b ->
            MemoryImage.FileSystem.update imageConfig initImage updateImage b model.image
                |> Tuple.mapBoth (\v -> { model | image = v }) (Cmd.map GotMemoryImageMsg)

        ExitSignal ->
            ( { model | status = Exiting }
            , MemoryImage.FileSystem.close |> Cmd.map GotMemoryImageMsg
            )

        Tick ->
            ( model
            , MemoryImage.FileSystem.sendMessage IncreaseCounter |> Cmd.map GotMemoryImageMsg
            )

        NoOperation ->
            ( model
            , Cmd.none
            )
    )
        |> (\( v, cmd ) ->
                let
                    _ =
                        Debug.log "Image" (v.image |> MemoryImage.FileSystem.image)
                in
                ( v
                , cmd
                )
           )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.status of
            Running ->
                Time.every 1000 (\_ -> Tick)

            Exiting ->
                Sub.none
        , MemoryImage.FileSystem.subscriptions model.image |> Sub.map GotMemoryImageMsg
        ]



--


type Status
    = Running
    | Exiting



--


type alias Image =
    { counter : Int
    }


initImage : () -> ( Image, Cmd ImageMsg )
initImage () =
    ( Image 0
    , Cmd.none
    )


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


updateImage : ImageMsg -> Image -> ( Image, Cmd ImageMsg )
updateImage msg model =
    case msg of
        IncreaseCounter ->
            ( { model | counter = model.counter + 1 }
            , Cmd.none
            )
