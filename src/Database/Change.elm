module Database.Change exposing
    ( Change, create, change, map
    , Merger, applyChange, applyFieldChange, applyAnyDictChange, applyFieldAnyDictChange
    )

{-|

@docs Change, create, change, map

@docs Merger, applyChange, applyFieldChange, applyAnyDictChange, applyFieldAnyDictChange

-}

import Dict.Any


type Change a
    = Change a a


create : (a -> a) -> a -> Change a
create fn a =
    Change a a |> change fn


change : (a -> a) -> Change a -> Change a
change fn (Change before after) =
    Change before (fn after)


map : (a -> b) -> Change a -> Change b
map fn (Change before after) =
    Change (fn before) (fn after)



--


type alias Merger a =
    Change a -> a -> a


applyChange : Merger a
applyChange (Change before after) current =
    if before == after then
        current

    else
        after


applyFieldChange : (a -> b) -> Change a -> a -> b
applyFieldChange fn change_ a =
    applyChange (map fn change_) (fn a)



--


applyAnyDictChange : (k -> comparable) -> Merger a -> Merger (Dict.Any.Dict k a)
applyAnyDictChange toComparable merger (Change before after) a =
    Dict.Any.merge
        toComparable
        (\k _ acc -> Dict.Any.remove toComparable k acc)
        (\k before_ after_ acc ->
            Dict.Any.update toComparable k (Maybe.map (merger (Change before_ after_))) acc
        )
        (\k v acc -> Dict.Any.insert toComparable k v acc)
        before
        after
        a


applyFieldAnyDictChange : (k -> comparable) -> Merger b -> (a -> Dict.Any.Dict k b) -> Change a -> a -> Dict.Any.Dict k b
applyFieldAnyDictChange toComparable merger fn change_ a =
    applyAnyDictChange toComparable merger (map fn change_) (fn a)
