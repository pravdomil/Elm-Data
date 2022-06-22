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


documentById : Database index a -> Id.Id a -> Maybe a
documentById db a =
    db |> documents |> Dict.Any.get Id.toString a


documentsByIndex : Config comparable index a -> Database index a -> index -> List a
documentsByIndex config db a =
    idsByIndex config db a |> List.filterMap (documentById db)


idsByIndex : Config comparable index a -> Database index a -> index -> List (Id.Id a)
idsByIndex config (Database index _) a =
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


type alias Config comparable index a =
    { toId : a -> Id.Id a
    , toIndexes : a -> List index
    , indexToComparable : index -> comparable
    }


insert : Config comparable index a -> a -> Database index a -> Database index a
insert config new (Database index db) =
    let
        id : Id.Id a
        id =
            config.toId new
    in
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


insertMany : Config comparable index a -> List a -> Database index a -> Database index a
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
    Codec.list a
        |> Codec.map
            (\v -> empty |> insertMany config v)
            (\(Database _ db) -> db |> Dict.Any.values)
