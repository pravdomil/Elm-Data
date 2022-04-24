module Database exposing (..)

import Codec
import Dict.Any
import Id


type Database index a
    = Database (Dict.Any.Dict ( index, Id.Id a ) ()) (Dict.Any.Dict (Id.Id a) a)


empty : Database index a
empty =
    Database Dict.Any.empty Dict.Any.empty


database : Database index a -> Dict.Any.Dict (Id.Id a) a
database (Database _ db) =
    db



--


readDocuments : (index -> comparable) -> index -> Database index a -> List a
readDocuments toComparable index_ ((Database _ db) as a) =
    readIds toComparable index_ a |> List.filterMap (\v -> db |> Dict.Any.get Id.toString v)


readIds : (index -> comparable) -> index -> Database index a -> List (Id.Id a)
readIds toComparable index_ (Database index _) =
    let
        toOrder : ( index, Id.Id a ) -> Order
        toOrder ( i, _ ) =
            compare (toComparable i) (toComparable index_)
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
