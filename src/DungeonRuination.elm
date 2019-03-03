module DungeonRuination exposing (dungeonRuinationView, select)

import Html exposing (Html)
import List.Extra as List
import Tables exposing (GenRandomMsg, SelectMsg, TableType(..), simpleView)


type alias DungeonRuinationModel m =
    { m | ruination : Maybe String }


dungeonRuination : List ( Int, String, Maybe x )
dungeonRuination =
    [ ( 1, "arcane disaster", Nothing )
    , ( 2, "damnation/curse", Nothing )
    , ( 4, "earthquake/fire/flood", Nothing )
    , ( 6, "plague/famine/drought", Nothing )
    , ( 8, "overrun by monsters", Nothing )
    , ( 10, "war/invasion", Nothing )
    , ( 11, "depleted resources", Nothing )
    , ( 12, "better prospects elsewhere", Nothing )
    ]


dungeonRuinationView :
    (DungeonRuinationModel m -> Maybe String)
    -> DungeonRuinationModel m
    -> SelectMsg msg
    -> GenRandomMsg msg
    -> List (Html msg)
dungeonRuinationView field model selectMsg genMsg =
    simpleView field model selectMsg genMsg "Ruination" DungeonRuination dungeonRuination


select : Int -> Maybe String
select offset =
    Maybe.map (\( _, n, _ ) -> n) (List.find (\( i, _, _ ) -> i >= offset) dungeonRuination)
