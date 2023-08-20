module Database exposing
    ( Database, empty
    , Config
    , documents, documentById, documentsByIndex, idsByIndex
    , insert, insertMany
    , remove, removeMany
    , codec
    )

{-|

@docs Database, empty

@docs Config

@docs documents, documentById, documentsByIndex, idsByIndex

@docs insert, insertMany

@docs remove, removeMany

@docs codec

-}

import Codec
import Dict.Any
import Id


type Database index a
    = Database (Dict.Any.Dict ( index, Id.Id a ) ()) (Dict.Any.Dict (Id.Id a) a)


empty : Database index a
empty =
    Database Dict.Any.empty Dict.Any.empty



--


type alias Config comparable index a =
    { toIndexes : a -> List index
    , indexToComparable : index -> comparable
    }



--


documents : Database index a -> Dict.Any.Dict (Id.Id a) a
documents (Database _ a) =
    a


documentById : Id.Id a -> Database index a -> Maybe a
documentById id a =
    Dict.Any.get Id.toString id (documents a)


documentsByIndex : Config comparable index a -> index -> Database index a -> List ( Id.Id a, a )
documentsByIndex config index a =
    idsByIndex config index a
        |> List.filterMap (\x -> Maybe.map (Tuple.pair x) (documentById x a))


idsByIndex : Config comparable index a -> index -> Database index a -> List (Id.Id a)
idsByIndex config a (Database index _) =
    let
        a_ : comparable
        a_ =
            config.indexToComparable a

        toOrder : ( index, Id.Id a ) -> Order
        toOrder ( i, _ ) =
            compare a_ (config.indexToComparable i)
    in
    Dict.Any.foldrByOrder toOrder (\( _, k ) _ acc -> k :: acc) [] index



--


insert : Config comparable index a -> ( Id.Id a, a ) -> Database index a -> Database index a
insert config ( id, new ) (Database index db) =
    Database
        (index
            |> (\x ->
                    case Dict.Any.get Id.toString id db of
                        Just doc ->
                            removeIndexForId config id doc x

                        Nothing ->
                            x
               )
            |> insertIndexForId config id new
        )
        (Dict.Any.insert Id.toString id new db)


insertMany : Config comparable index a -> List ( Id.Id a, a ) -> Database index a -> Database index a
insertMany config docs a =
    docs |> List.foldl (insert config) a



--


remove : Config comparable index a -> Id.Id a -> Database index a -> Database index a
remove config id (Database index db) =
    case db |> Dict.Any.get Id.toString id of
        Just old ->
            Database
                (removeIndexForId config id old index)
                (Dict.Any.remove Id.toString id db)

        Nothing ->
            Database index db


removeMany : Config comparable index a -> List (Id.Id a) -> Database index a -> Database index a
removeMany config ids a =
    ids |> List.foldl (remove config) a



--


codec : Config comparable index a -> Codec.Codec a -> Codec.Codec (Database index a)
codec config a =
    Codec.list (Codec.tuple Id.codec a)
        |> Codec.map
            (\x -> x |> documents |> Dict.Any.toList)
            (\x -> empty |> insertMany config x)



--


insertIndexForId : Config comparable index a -> Id.Id a -> a -> Dict.Any.Dict ( index, Id.Id a ) () -> Dict.Any.Dict ( index, Id.Id a ) ()
insertIndexForId config id a index =
    List.foldl
        (\x acc ->
            Dict.Any.insert (Tuple.mapBoth config.indexToComparable Id.toString) ( x, id ) () acc
        )
        index
        (config.toIndexes a)


removeIndexForId : Config comparable index a -> Id.Id a -> a -> Dict.Any.Dict ( index, Id.Id a ) () -> Dict.Any.Dict ( index, Id.Id a ) ()
removeIndexForId config id a index =
    List.foldl
        (\x acc ->
            Dict.Any.remove (Tuple.mapBoth config.indexToComparable Id.toString) ( x, id ) acc
        )
        index
        (config.toIndexes a)
