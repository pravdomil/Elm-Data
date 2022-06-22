module Database.Change exposing
    ( Change, create, change, before, after, map, codec
    , Applier, applyChange, applyFieldChange, applyAnyDictChange, applyFieldAnyDictChange
    )

{-|

@docs Change, create, change, before, after, map, codec

@docs Applier, applyChange, applyFieldChange, applyAnyDictChange, applyFieldAnyDictChange

-}

import Codec
import Dict.Any


type Change a
    = Change a a


create : (a -> a) -> a -> Change a
create fn a =
    Change a a |> change fn


change : (a -> a) -> Change a -> Change a
change fn (Change before_ after_) =
    Change before_ (fn after_)


map : (a -> b) -> Change a -> Change b
map fn (Change before_ after_) =
    Change (fn before_) (fn after_)


before : Change a -> a
before (Change a _) =
    a


after : Change a -> a
after (Change _ a) =
    a


codec : Codec.Codec a -> Codec.Codec (Change a)
codec a =
    Codec.custom
        (\fn1 v ->
            case v of
                Change v1 v2 ->
                    fn1 v1 v2
        )
        |> Codec.variant2 "Change" Change a a
        |> Codec.buildCustom



--


type alias Applier a =
    Change a -> a -> a


applyChange : Applier a
applyChange (Change before_ after_) current =
    if before_ == after_ then
        current

    else
        after_


applyFieldChange : (a -> b) -> Change a -> a -> b
applyFieldChange fn change_ a =
    applyChange (map fn change_) (fn a)



--


applyAnyDictChange : (k -> comparable) -> Applier a -> Applier (Dict.Any.Dict k a)
applyAnyDictChange toComparable applier (Change before_ after_) a =
    Dict.Any.merge
        toComparable
        (\k _ acc -> Dict.Any.remove toComparable k acc)
        (\k before__ after__ acc ->
            Dict.Any.update toComparable k (Maybe.map (applier (Change before__ after__))) acc
        )
        (\k v acc -> Dict.Any.insert toComparable k v acc)
        before_
        after_
        a


applyFieldAnyDictChange : (k -> comparable) -> Applier b -> (a -> Dict.Any.Dict k b) -> Change a -> a -> Dict.Any.Dict k b
applyFieldAnyDictChange toComparable applier fn change_ a =
    applyAnyDictChange toComparable applier (map fn change_) (fn a)
