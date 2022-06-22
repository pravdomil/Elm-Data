module Database.Change exposing
    ( Change, create, change, before, after, map
    , Merger, applyChange, applyFieldChange, applyAnyDictChange, applyFieldAnyDictChange
    )

{-|

@docs Change, create, change, before, after, map

@docs Merger, applyChange, applyFieldChange, applyAnyDictChange, applyFieldAnyDictChange

-}

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



--


type alias Merger a =
    Change a -> a -> a


applyChange : Merger a
applyChange (Change before_ after_) current =
    if before_ == after_ then
        current

    else
        after_


applyFieldChange : (a -> b) -> Change a -> a -> b
applyFieldChange fn change_ a =
    applyChange (map fn change_) (fn a)



--


applyAnyDictChange : (k -> comparable) -> Merger a -> Merger (Dict.Any.Dict k a)
applyAnyDictChange toComparable merger (Change before_ after_) a =
    Dict.Any.merge
        toComparable
        (\k _ acc -> Dict.Any.remove toComparable k acc)
        (\k before__ after__ acc ->
            Dict.Any.update toComparable k (Maybe.map (merger (Change before__ after__))) acc
        )
        (\k v acc -> Dict.Any.insert toComparable k v acc)
        before_
        after_
        a


applyFieldAnyDictChange : (k -> comparable) -> Merger b -> (a -> Dict.Any.Dict k b) -> Change a -> a -> Dict.Any.Dict k b
applyFieldAnyDictChange toComparable merger fn change_ a =
    applyAnyDictChange toComparable merger (map fn change_) (fn a)
