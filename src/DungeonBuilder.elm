module DungeonBuilder exposing (dungeonBuilderView, select)

import Html exposing (Html)
import Humanoid exposing (HumanoidModel)
import List.Extra as List
import Tables exposing (Count(..), GenRandomMsg, TableMsg, TableType(..), simpleWithChildrenView)


type alias DungeonBuilderModel m =
    { m
        | builder : Maybe String
        , humanoid : Maybe String
        , beast1 : Maybe String
        , beast2 : Maybe String
    }


dungeonBuilder :
    List
        ( Int
        , String
        , Maybe
            (Count
                (HumanoidModel m
                 -> TableMsg msg
                 -> GenRandomMsg msg
                 -> List (Html msg)
                )
            )
        )
dungeonBuilder =
    [ ( 1, "aliens/precursors", Nothing )
    , ( 2, "demigod/demon", Nothing )
    , ( 4, "natural (caves, etc.)", Nothing )
    , ( 5, "religious order/cult", Nothing )
    , ( 7, "humanoid", Just (Single (Humanoid.humanoidView .humanoid)) )
    , ( 9, "dwarves/gnomes", Nothing )
    , ( 10, "elves", Nothing )
    , ( 11, "wizard/madman", Nothing )
    , ( 12, "monarch/warlord", Nothing )
    ]


dungeonBuilderView :
    (DungeonBuilderModel m -> Maybe String)
    -> DungeonBuilderModel m
    -> TableMsg msg
    -> GenRandomMsg msg
    -> List (Html msg)
dungeonBuilderView field model selectMsg genMsg =
    simpleWithChildrenView field model selectMsg genMsg "Builder" DungeonBuilder dungeonBuilder


select : Int -> Maybe String
select offset =
    Maybe.map (\( _, n, _ ) -> n) (List.find (\( i, _, _ ) -> i >= offset) dungeonBuilder)
