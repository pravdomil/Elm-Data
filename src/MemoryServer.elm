module MemoryServer exposing (..)

import FileSystem
import Http.Server
import Http.Server.Internals
import MemoryImage
import MemoryImage.FileSystem
import Process.Extra


worker : Config msg a -> Program () (Model msg a) (Msg msg)
worker config =
    Platform.worker
        { init = init config
        , update = update config
        , subscriptions = subscriptions config
        }



--


type alias Config msg a =
    { init : () -> ( a, Cmd msg )
    , update : msg -> a -> ( a, Cmd msg )
    , subscriptions : a -> Sub msg

    --
    , image : MemoryImage.Config msg a
    , imagePath : FileSystem.Path

    --
    , serverOptions : Http.Server.Internals.ListenOptions
    , gotRequest : Http.Server.Internals.Request -> msg
    }



--


type alias Model msg a =
    { server : Http.Server.Server
    , image : MemoryImage.FileSystem.Image msg a
    }


init : Config msg a -> () -> ( Model msg a, Cmd (Msg msg) )
init config () =
    let
        ( server, cmd ) =
            Http.Server.init config.serverOptions

        ( image, cmd2 ) =
            MemoryImage.FileSystem.init config.image config.imagePath
    in
    ( Model server image
    , Cmd.batch
        [ cmd |> Cmd.map GotServerMsg
        , cmd2 |> Cmd.map GotMemoryImageMsg
        , Process.Extra.onExitSignal ExitSignal
        ]
    )



--


type Msg msg
    = GotServerMsg Http.Server.Msg
    | GotMemoryImageMsg (MemoryImage.FileSystem.Msg msg)
    | ExitSignal
    | NoOperation


update : Config msg a -> Msg msg -> Model msg a -> ( Model msg a, Cmd (Msg msg) )
update config msg model =
    case msg of
        GotServerMsg b ->
            case Http.Server.toPublicMsg b of
                Just c ->
                    case c of
                        Http.Server.GotRequest d ->
                            ( model
                            , MemoryImage.FileSystem.sendMessage (config.gotRequest d)
                                |> Cmd.map GotMemoryImageMsg
                            )

                Nothing ->
                    Http.Server.update b model.server
                        |> Tuple.mapBoth (\v -> { model | server = v }) (Cmd.map GotServerMsg)

        GotMemoryImageMsg b ->
            MemoryImage.FileSystem.update config.image config.init config.update b model.image
                |> Tuple.mapBoth (\v -> { model | image = v }) (Cmd.map GotMemoryImageMsg)

        ExitSignal ->
            ( model
            , Cmd.batch
                [ Http.Server.close
                    |> Cmd.map GotServerMsg
                , MemoryImage.FileSystem.close
                    |> Cmd.map GotMemoryImageMsg
                ]
            )

        NoOperation ->
            ( model
            , Cmd.none
            )



--


subscriptions : Config msg a -> Model msg a -> Sub (Msg msg)
subscriptions _ model =
    Sub.batch
        [ Http.Server.subscriptions model.server |> Sub.map GotServerMsg
        , MemoryImage.FileSystem.subscriptions model.image |> Sub.map GotMemoryImageMsg
        ]
