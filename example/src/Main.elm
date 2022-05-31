module Main exposing (..)

import Codec
import Console
import FileSystem
import MemoryImage
import MemoryImage.Worker
import Task
import Time


main =
    MemoryImage.Worker.worker
        { image = MemoryImage.Config (Codec.encoder modelCodec) (Codec.decoder modelCodec) (Codec.encoder msgCodec) (Codec.decoder msgCodec)
        , imagePath = FileSystem.Path "image.jsonl"

        --
        , init = init
        , update = update
        , subscriptions = subscriptions

        --
        , gotFlags = GotFlags
        }



--


type alias Model =
    { counter : Int
    }


modelCodec : Codec.Codec Model
modelCodec =
    Codec.object Model
        |> Codec.field "counter" .counter Codec.int
        |> Codec.buildObject


init : () -> ( Model, Cmd Msg )
init () =
    ( Model 1
    , Cmd.none
    )



--


type Msg
    = GotFlags ()
    | IncreaseCounter
    | NoOperation


msgCodec : Codec.Codec Msg
msgCodec =
    Codec.custom
        (\fn1 fn2 fn3 v ->
            case v of
                GotFlags v1 ->
                    fn1 v1

                IncreaseCounter ->
                    fn2

                NoOperation ->
                    fn3
        )
        |> Codec.variant1 "GotFlags" GotFlags (Codec.succeed ())
        |> Codec.variant0 "IncreaseCounter" IncreaseCounter
        |> Codec.variant0 "NoOperation" NoOperation
        |> Codec.buildCustom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlags _ ->
            ( model
            , Cmd.none
            )

        IncreaseCounter ->
            let
                nextModel : Model
                nextModel =
                    { model | counter = model.counter + 1 }
            in
            ( nextModel
            , Console.log (String.fromInt nextModel.counter)
                |> Task.attempt (\_ -> NoOperation)
            )

        NoOperation ->
            ( model
            , Cmd.none
            )



--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 10 (\_ -> IncreaseCounter)
