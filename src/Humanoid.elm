module Humanoid exposing (HumanoidModel, humanoidView, select)

import Beast exposing (Beast2Model, beast1View, beast2View)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Tables exposing (Count(..), GenRandomMsg, TableMsg, TableType(..), complexWithChildrenView)


type alias HumanoidModel m =
    { m
        | humanoid : Maybe String
        , beast1 : Maybe String
        , beast2 : Maybe String
    }


humanoid :
    List
        ( String
        , Int
        , List
            ( Int
            , String
            , Maybe
                (Count
                    (Beast2Model m
                     -> TableMsg msg
                     -> GenRandomMsg msg
                     -> List (Html msg)
                    )
                )
            )
        )
humanoid =
    [ ( "Common"
      , 7
      , [ ( 3, "halfling", Nothing )
        , ( 5, "goblin/kobold", Nothing )
        , ( 7, "dwarf/gnome", Nothing )
        , ( 9, "orc/hobgoblin/gnoll", Nothing )
        , ( 11, "half-elf/half-orc, etc.", Nothing )
        , ( 12, "elf", Nothing )
        ]
      )
    , ( "Uncommon"
      , 10
      , [ ( 1, "fey", Nothing )
        , ( 3, "catfolk/dogfolk", Nothing )
        , ( 6, "lizardfolk/merfolk", Nothing )
        , ( 7, "birdfolk", Nothing )
        , ( 10, "ogre/troll", Nothing )
        , ( 12, "cyclops/giant", Nothing )
        ]
      )
    , ( "Hybrid"
      , 12
      , [ ( 2, "centaur", Nothing )
        , ( 5, "werewolf/werebear", Nothing )
        , ( 6, "werecreature", Just (Single (beast1View .beast1)) )
        , ( 10, "human + BEAST", Just (Single (beast1View .beast1)) )
        , ( 12, "human + 2 BEASTS", Just (Double (beast1View .beast1) (beast2View .beast2)) )
        ]
      )
    ]


humanoidView :
    (HumanoidModel m -> Maybe String)
    -> HumanoidModel m
    -> TableMsg msg
    -> GenRandomMsg msg
    -> List (Html msg)
humanoidView field model selectMsg genMsg =
    complexWithChildrenView field model selectMsg genMsg "Humanoid" Humanoid humanoid


select : ( Int, Int ) -> Maybe String
select offset =
    List.find (\( _, f, _ ) -> f >= Tuple.first offset) humanoid
        |> Maybe.map
            (\( _, _, l ) ->
                List.find (\( s, _, _ ) -> s >= Tuple.second offset) l
            )
        |> Maybe.join
        |> Maybe.map (\( _, n, _ ) -> n)
