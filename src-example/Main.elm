module Main exposing (..)

import FileSystem
import MemoryImage.FileSystem
import Model
import Platform.Extra
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
    { image : MemoryImage.FileSystem.Image Model.Msg Model.Model
    , status : Status
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( image, cmd ) =
            MemoryImage.FileSystem.init (FileSystem.Path "image.jsonl")
    in
    ( Model image Running
    , Cmd.batch
        [ cmd |> Cmd.map MessageReceived
        , Process.Extra.onExitSignal ExitSignalReceived
        ]
    )



--


type Status
    = Running
    | Exiting



--


type Msg
    = NothingHappened
    | MessageReceived (MemoryImage.FileSystem.Msg Model.Msg)
    | SecondElapsed
    | ExitSignalReceived


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        MessageReceived b ->
            MemoryImage.FileSystem.update Model.config Model.config2 b model.image
                |> Tuple.mapBoth (\x -> { model | image = x }) (Cmd.map MessageReceived)

        SecondElapsed ->
            MemoryImage.FileSystem.sendMessage Model.config Model.config2 Model.IncreaseCounter model.image
                |> Tuple.mapBoth (\x -> { model | image = x }) (Cmd.map MessageReceived)

        ExitSignalReceived ->
            ( { model | status = Exiting }
            , Cmd.none
            )
                |> Platform.Extra.andThen
                    (\x ->
                        MemoryImage.FileSystem.close Model.config Model.config2 x.image
                            |> Tuple.mapBoth (\x2 -> { x | image = x2 }) (Cmd.map MessageReceived)
                    )
    )
        |> (\( x, cmd ) ->
                let
                    _ =
                        Debug.log "" ""

                    _ =
                        Debug.log "Message" msg

                    _ =
                        Debug.log "Image" (x.image |> MemoryImage.FileSystem.image)
                in
                ( x
                , cmd
                )
           )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.status of
            Running ->
                Time.every 1000 (\_ -> SecondElapsed)

            Exiting ->
                Sub.none
        , MemoryImage.FileSystem.subscriptions model.image
            |> Sub.map MessageReceived
        ]
