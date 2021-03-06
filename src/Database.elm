module Database exposing
    ( Database, empty, codec, Config
    , documentById, documentsByIndex, idsByIndex, documents
    , insert, insertMany
    , remove, removeMany
    )

{-|

@docs Database, empty, codec, Config

@docs documentById, documentsByIndex, idsByIndex, documents

@docs insert, insertMany

@docs remove, removeMany

-}

import Codec
import Dict.Any
import Id


type Database index a
    = Database (Dict.Any.Dict ( index, Id.Id a ) ()) (Dict.Any.Dict (Id.Id a) a)


empty : Database index a
empty =
    Database Dict.Any.empty Dict.Any.empty


documents : Database index a -> Dict.Any.Dict (Id.Id a) a
documents (Database _ db) =
    db



--


documentById : Id.Id a -> Database index a -> Maybe ( Id.Id a, a )
documentById id a =
    a |> documents |> Dict.Any.get Id.toString id |> Maybe.map (Tuple.pair id)


documentsByIndex : Config comparable index a -> index -> Database index a -> List ( Id.Id a, a )
documentsByIndex config index a =
    idsByIndex config index a |> List.filterMap (\x -> documentById x a)


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


type alias Config comparable index a =
    { toIndexes : a -> List index
    , indexToComparable : index -> comparable
    }



--


insert : Config comparable index a -> ( Id.Id a, a ) -> Database index a -> Database index a
insert config ( id, new ) (Database index db) =
    case db |> Dict.Any.get Id.toString id of
        Just old ->
            Database
                (config.toIndexes old
                    |> List.foldl
                        (\v acc ->
                            acc
                                |> Dict.Any.remove
                                    (Tuple.mapBoth config.indexToComparable Id.toString)
                                    ( v, id )
                        )
                        index
                    |> (\v ->
                            List.foldl
                                (\v2 acc ->
                                    acc
                                        |> Dict.Any.insert
                                            (Tuple.mapBoth config.indexToComparable Id.toString)
                                            ( v2, id )
                                            ()
                                )
                                v
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
                        (\v acc ->
                            acc
                                |> Dict.Any.insert
                                    (Tuple.mapBoth config.indexToComparable Id.toString)
                                    ( v, id )
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
                        (\v acc ->
                            acc
                                |> Dict.Any.remove
                                    (Tuple.mapBoth config.indexToComparable Id.toString)
                                    ( v, id )
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
            (\(Database _ db) -> db |> Dict.Any.toList)
            (\v -> empty |> insertMany config v)
