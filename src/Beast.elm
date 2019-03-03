module Beast exposing (Beast2Model, beast1View, beast2View, select)

import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Tables exposing (Count(..), GenRandomMsg, SelectMsg, TableType(..), complexView)


type alias Beast1Model m =
    { m | beast1 : Maybe String }


type alias Beast2Model m =
    { m
        | beast1 : Maybe String
        , beast2 : Maybe String
    }


beast : List ( String, Int, List ( Int, String, Maybe x ) )
beast =
    [ ( "Earthbound"
      , 7
      , [ ( 1, "termite/tick/louse", Nothing )
        , ( 2, "snail/slug/worm", Nothing )
        , ( 3, "ant/centipede/scorpion", Nothing )
        , ( 4, "snake/lizard", Nothing )
        , ( 5, "vole/rat/weasel", Nothing )
        , ( 6, "boar/pig", Nothing )
        , ( 7, "dog/fox/wolf", Nothing )
        , ( 8, "cat/lion/panther", Nothing )
        , ( 9, "deer/horse/camel", Nothing )
        , ( 10, "ox/rhino", Nothing )
        , ( 11, "bear/ape/gorilla", Nothing )
        , ( 12, "mammoth/dinosaur", Nothing )
        ]
      )
    , ( "Airborne"
      , 10
      , [ ( 1, "mosquito/firefly", Nothing )
        , ( 2, "locust/dragonfly/moth", Nothing )
        , ( 3, "bee/wasp", Nothing )
        , ( 4, "chicken/duck/goose", Nothing )
        , ( 5, "songbird/parrot", Nothing )
        , ( 6, "gull/waterbird", Nothing )
        , ( 7, "heron/crane/stork", Nothing )
        , ( 8, "crow/raven", Nothing )
        , ( 9, "hawk/falcon", Nothing )
        , ( 10, "eagle/owl", Nothing )
        , ( 11, "condor", Nothing )
        , ( 12, "pteranodon", Nothing )
        ]
      )
    , ( "Water-going"
      , 12
      , [ ( 1, "insect", Nothing )
        , ( 2, "jelly/anemone", Nothing )
        , ( 3, "clam/oyster/snail", Nothing )
        , ( 4, "eel/snake", Nothing )
        , ( 5, "frog/toad", Nothing )
        , ( 6, "fish", Nothing )
        , ( 7, "crab/lobster", Nothing )
        , ( 8, "turtle", Nothing )
        , ( 9, "alligator/crocodile", Nothing )
        , ( 10, "dolphin/shark", Nothing )
        , ( 11, "squid/octopus", Nothing )
        , ( 12, "whale", Nothing )
        ]
      )
    ]


beast1View :
    (Beast1Model m -> Maybe String)
    -> Beast1Model m
    -> SelectMsg msg
    -> GenRandomMsg msg
    -> List (Html msg)
beast1View field model selectMsg genMsg =
    complexView field model selectMsg genMsg "Beast" Beast1 beast


beast2View :
    (Beast2Model m -> Maybe String)
    -> Beast2Model m
    -> SelectMsg msg
    -> GenRandomMsg msg
    -> List (Html msg)
beast2View field model selectMsg genMsg =
    complexView field model selectMsg genMsg "Beast" Beast2 beast


select : ( Int, Int ) -> Maybe String
select offset =
    List.find (\( _, f, _ ) -> f >= Tuple.first offset) beast
        |> Maybe.map
            (\( _, _, l ) ->
                List.find (\( s, _, _ ) -> s >= Tuple.second offset) l
            )
        |> Maybe.join
        |> Maybe.map (\( _, n, _ ) -> n)
