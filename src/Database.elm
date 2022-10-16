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
documents (Database _ db) =
    db


documentById : Id.Id a -> Database index a -> Maybe a
documentById id a =
    a |> documents |> Dict.Any.get Id.toString id


documentsByIndex : Config comparable index a -> index -> Database index a -> List ( Id.Id a, a )
documentsByIndex config index a =
    idsByIndex config index a |> List.filterMap (\x -> documentById x a |> Maybe.map (Tuple.pair x))


idsByIndex : Config comparable index a -> index -> Database index a -> List (Id.Id a)
idsByIndex config a (Database index _) =
    let
        index__ : comparable
        index__ =
            config.indexToComparable a

        toOrder : ( index, Id.Id a ) -> Order
        toOrder ( i, _ ) =
            compare index__ (config.indexToComparable i)
    in
    index |> Dict.Any.foldrByOrder toOrder (\( _, k ) _ acc -> k :: acc) []



--


insert : Config comparable index a -> ( Id.Id a, a ) -> Database index a -> Database index a
insert config ( id, new ) (Database index db) =
    case db |> Dict.Any.get Id.toString id of
        Just old ->
            Database
                (config.toIndexes old
                    |> List.foldl
                        (\x acc ->
                            acc
                                |> Dict.Any.remove
                                    (Tuple.mapBoth config.indexToComparable Id.toString)
                                    ( x, id )
                        )
                        index
                    |> (\x ->
                            List.foldl
                                (\x2 acc ->
                                    acc
                                        |> Dict.Any.insert
                                            (Tuple.mapBoth config.indexToComparable Id.toString)
                                            ( x2, id )
                                            ()
                                )
                                x
                                (config.toIndexes new)
                       )
                )
                (db
                    |> Dict.Any.insert
                        Id.toString
                        id
                        new
                )

        Nothing ->
            Database
                (config.toIndexes new
                    |> List.foldl
                        (\x acc ->
                            acc
                                |> Dict.Any.insert
                                    (Tuple.mapBoth config.indexToComparable Id.toString)
                                    ( x, id )
                                    ()
                        )
                        index
                )
                (db
                    |> Dict.Any.insert
                        Id.toString
                        id
                        new
                )


insertMany : Config comparable index a -> List ( Id.Id a, a ) -> Database index a -> Database index a
insertMany config docs a =
    docs |> List.foldl (insert config) a



--


remove : Config comparable index a -> Id.Id a -> Database index a -> Database index a
remove config id (Database index db) =
    case db |> Dict.Any.get Id.toString id of
        Just old ->
            Database
                (config.toIndexes old
                    |> List.foldl
                        (\x acc ->
                            acc
                                |> Dict.Any.remove
                                    (Tuple.mapBoth config.indexToComparable Id.toString)
                                    ( x, id )
                        )
                        index
                )
                (db
                    |> Dict.Any.remove
                        Id.toString
                        id
                )

        Nothing ->
            Database
                index
                db


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
