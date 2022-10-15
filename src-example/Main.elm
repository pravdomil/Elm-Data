module Main exposing (..)

import FileSystem
import MemoryImage.FileSystem
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
    | MessageReceived (MemoryImage.FileSystem.Msg ImageMsg)
    | ExitSignalReceived
    | SecondElapsed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NothingHappened ->
            Platform.Extra.noOperation model

        MessageReceived b ->
            MemoryImage.FileSystem.update imageConfig initImage updateImage b model.image
                |> Tuple.mapBoth (\v -> { model | image = v }) (Cmd.map MessageReceived)

        ExitSignalReceived ->
            ( { model | status = Exiting }
            , MemoryImage.FileSystem.close |> Cmd.map MessageReceived
            )

        SecondElapsed ->
            ( model
            , MemoryImage.FileSystem.sendMessage IncreaseCounter |> Cmd.map MessageReceived
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
                Time.every 1000 (\_ -> SecondElapsed)

            Exiting ->
                Sub.none
        , MemoryImage.FileSystem.subscriptions model.image |> Sub.map MessageReceived
        ]
