module DungeonFunction exposing (dungeonFunctionView, select)

import Html exposing (Html)
import List.Extra as List
import Tables exposing (GenRandomMsg, TableMsg, TableType(..), simpleView)


type alias DungeonFunctionModel m =
    { m | function : Maybe String }


dungeonFunction : List ( Int, String, Maybe x )
dungeonFunction =
    [ ( 1, "source/portal", Nothing )
    , ( 2, "mine", Nothing )
    , ( 4, "tomb/crypt", Nothing )
    , ( 5, "prison", Nothing )
    , ( 7, "lair/den/hideout", Nothing )
    , ( 9, "stronghold/sanctuary", Nothing )
    , ( 10, "shrine/temple/oracle", Nothing )
    , ( 11, "archive/library", Nothing )
    , ( 12, "unknown/mystery", Nothing )
    ]


dungeonFunctionView :
    (DungeonFunctionModel m -> Maybe String)
    -> DungeonFunctionModel m
    -> TableMsg msg
    -> GenRandomMsg msg
    -> List (Html msg)
dungeonFunctionView field model selectMsg genMsg =
    simpleView field model selectMsg genMsg "Function" DungeonFunction dungeonFunction


select : Int -> Maybe String
select offset =
    Maybe.map (\( _, n, _ ) -> n) (List.find (\( i, _, _ ) -> i >= offset) dungeonFunction)
