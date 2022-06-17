module Database exposing
    ( Database, empty, codec, Config
    , readById, readByIndex, readIdsByIndex, documents
    , insert, insertMany
    , remove, removeMany
    )

{-|

@docs Database, empty, codec, Config

@docs readById, readByIndex, readIdsByIndex, documents

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


readById : Database index a -> Id.Id a -> Maybe a
readById db a =
    db |> documents |> Dict.Any.get Id.toString a


readByIndex : (index -> comparable) -> Database index a -> index -> List a
readByIndex toComparable db a =
    readIdsByIndex toComparable db a |> List.filterMap (readById db)


readIdsByIndex : (index -> comparable) -> Database index a -> index -> List (Id.Id a)
readIdsByIndex toComparable (Database index _) a =
    let
        index__ : comparable
        index__ =
            toComparable a

        toOrder : ( index, Id.Id a ) -> Order
        toOrder ( i, _ ) =
            compare index__ (toComparable i)
    in
    index |> Dict.Any.foldrByOrder toOrder (\( _, k ) _ acc -> k :: acc) []



--


type alias Config index a =
    { toId : a -> Id.Id a
    , toIndexes : a -> List index
    }


insert : Config index a -> (index -> comparable) -> a -> Database index a -> Database index a
insert config toComparable new (Database index db) =
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
                                    (Tuple.mapBoth toComparable Id.toString)
                                    ( v, id )
                        )
                        index
                    |> (\v ->
                            List.foldl
                                (\v2 acc ->
                                    acc
                                        |> Dict.Any.insert
                                            (Tuple.mapBoth toComparable Id.toString)
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
                                    (Tuple.mapBoth toComparable Id.toString)
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


insertMany : Config index a -> (index -> comparable) -> List a -> Database index a -> Database index a
insertMany config toComparable docs a =
    docs |> List.foldl (insert config toComparable) a



--


remove : Config index a -> (index -> comparable) -> Id.Id a -> Database index a -> Database index a
remove config toComparable id (Database index db) =
    case db |> Dict.Any.get Id.toString id of
        Just old ->
            Database
                (config.toIndexes old
                    |> List.foldl
                        (\v acc ->
                            acc
                                |> Dict.Any.remove
                                    (Tuple.mapBoth toComparable Id.toString)
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


removeMany : Config index a -> (index -> comparable) -> List (Id.Id a) -> Database index a -> Database index a
removeMany config toComparable ids a =
    ids |> List.foldl (remove config toComparable) a



--


codec : Config index a -> (index -> comparable) -> Codec.Codec a -> Codec.Codec (Database index a)
codec config toComparable a =
    Codec.list a
        |> Codec.map
            (\v -> empty |> insertMany config toComparable v)
            (\(Database _ db) -> db |> Dict.Any.values)
