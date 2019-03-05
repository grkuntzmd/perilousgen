module DungeonTheme exposing (DungeonThemeModel, dungeonTheme, dungeonThemeView, select)

import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Tables exposing (Count(..), GenRandomMsg, SelectMsg, TableType(..), complexView)


type alias DungeonThemeModel m =
    { m
        | theme : Maybe String
    }


dungeonTheme : List ( String, Int, List ( Int, String, Maybe x ) )
dungeonTheme =
    [ ( "Mundane"
      , 5
      , [ ( 1, "rot/decay", Nothing )
        , ( 2, "torture/agony", Nothing )
        , ( 3, "madness", Nothing )
        , ( 4, "all is lost", Nothing )
        , ( 5, "noble sacrifice", Nothing )
        , ( 6, "savage fury", Nothing )
        , ( 7, "survival", Nothing )
        , ( 8, "criminal activity", Nothing )
        , ( 9, "secrets/treachery", Nothing )
        , ( 10, "tricks and traps", Nothing )
        , ( 11, "invasion/infestation", Nothing )
        , ( 12, "factions at war", Nothing )
        ]
      )
    , ( "Unusual"
      , 9
      , [ ( 1, "creation/invention", Nothing )
        , ( 2, "element", Nothing )
        , ( 3, "knowledge/learning", Nothing )
        , ( 4, "growth/expansion", Nothing )
        , ( 5, "deepening mystery", Nothing )
        , ( 6, "transformation/change", Nothing )
        , ( 7, "chaos and destruction", Nothing )
        , ( 8, "shadowy forces", Nothing )
        , ( 9, "forbidden knowledge", Nothing )
        , ( 10, "poison/disease", Nothing )
        , ( 11, "corruption/blight", Nothing )
        , ( 12, "impending disaster", Nothing )
        ]
      )
    , ( "Extraordinary"
      , 12
      , [ ( 1, "scheming evil", Nothing )
        , ( 2, "divination/scrying", Nothing )
        , ( 3, "blasphemy", Nothing )
        , ( 4, "arcane research", Nothing )
        , ( 5, "occult forces", Nothing )
        , ( 6, "an ancient curse", Nothing )
        , ( 7, "mutation", Nothing )
        , ( 8, "the unquiet dead", Nothing )
        , ( 9, "bottomless hunger", Nothing )
        , ( 10, "incredible power", Nothing )
        , ( 11, "unspeakable horrors", Nothing )
        , ( 12, "holy war", Nothing )
        ]
      )
    ]


dungeonThemeView :
    (DungeonThemeModel m -> Maybe String)
    -> DungeonThemeModel m
    -> SelectMsg msg
    -> GenRandomMsg msg
    -> List (Html msg)
dungeonThemeView field model selectMsg genMsg =
    complexView field model selectMsg genMsg "Theme" DungeonTheme dungeonTheme


select : ( Int, Int ) -> Maybe String
select offset =
    List.find (\( _, f, _ ) -> f >= Tuple.first offset) dungeonTheme
        |> Maybe.map
            (\( _, _, l ) ->
                List.find (\( s, _, _ ) -> s >= Tuple.second offset) l
            )
        |> Maybe.join
        |> Maybe.map (\( _, n, _ ) -> n)
