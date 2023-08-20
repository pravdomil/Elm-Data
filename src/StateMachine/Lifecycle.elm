module StateMachine.Lifecycle exposing (..)

import Codec


type Lifecycle
    = Running
    | Exiting


codec : Codec.Codec Lifecycle
codec =
    Codec.custom
        (\fn1 fn2 x ->
            case x of
                Running ->
                    fn1

                Exiting ->
                    fn2
        )
        |> Codec.variant0 Running
        |> Codec.variant0 Exiting
        |> Codec.buildCustom
