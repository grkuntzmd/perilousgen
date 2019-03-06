module DungeonTheme exposing
    ( DungeonThemeModel
    , DungeonThemeTracker
    , dungeonTheme
    , dungeonThemeView
    , initTracker
    , select
    )

import DungeonMsg exposing (Msg(..))
import Element exposing (ElementModel, elementView)
import Html exposing (Html, a, button, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, type_)
import Html.Events exposing (onClick)
import Icons exposing (dice)
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Tables exposing (Count(..), GenRandomMsg, SelectMsg, TableType(..))


type alias DungeonThemeModel m =
    { m
        | theme : Maybe String
        , element : Maybe String
    }


type alias DungeonThemeTracker =
    { start : Int
    , stop : Int
    , stopText : String
    , checked : Set Int
    , name : Maybe String
    , element : Maybe String
    }


initTracker : Maybe String -> DungeonThemeTracker
initTracker name =
    { start = 1
    , stop = 1
    , stopText = "1"
    , checked = Set.empty
    , name = name
    , element = Nothing
    }


dungeonTheme :
    List
        ( String
        , Int
        , List
            ( Int
            , String
            , Maybe
                (Count
                    (ElementModel m
                     -> SelectMsg msg
                     -> GenRandomMsg msg
                     -> List (Html msg)
                    )
                )
            )
        )
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
        , ( 2, "element", Just (Single (elementView .element)) )
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
    Int
    -> DungeonThemeTracker
    -> List (Html Msg)
dungeonThemeView index tracker =
    let
        name =
            tracker.name

        entry =
            List.find (\( _, n, _ ) -> Just n == name) (List.concatMap (\( _, _, l ) -> l) dungeonTheme)

        items =
            List.concat <|
                List.map
                    (\( heading, _, entries ) ->
                        li [ class "uk-nav-header" ] [ text heading ]
                            :: List.map
                                (\( _, n, _ ) ->
                                    li
                                        [ classList [ ( "uk-active", Maybe.unwrap False ((==) n) name ) ] ]
                                        [ a
                                            [ href "/perilousgen#dungeon"
                                            , onClick (SelectThemeMsg index n)
                                            ]
                                            [ text n ]
                                        ]
                                )
                                entries
                    )
                    dungeonTheme
    in
    li []
        [ div [ class "uk-inline" ]
            [ span [ onClick (GenThemeMsg index) ] [ Icons.dice ]
            , button
                [ class "uk-button uk-button-default uk-margin-small-left uk-button-text", type_ "button" ]
                [ text ("Theme " ++ String.fromInt (index + 1)) ]
            , div
                [ attribute "uk-dropdown" "mode: click", class "uk-dropdown" ]
                [ ul [ class "uk-nav uk-dropdown-nav" ] items ]
            , span [ class "uk-label uk-margin-small-left" ] [ text (Maybe.withDefault "none" name) ]
            ]
        ]
        :: (case entry of
                Just ( _, _, Just (Single t) ) ->
                    t tracker (SelectElementMsg index) (GenRandomElementMsg index)

                _ ->
                    []
           )


select : ( Int, Int ) -> Maybe String
select offset =
    List.find (\( _, f, _ ) -> f >= Tuple.first offset) dungeonTheme
        |> Maybe.map
            (\( _, _, l ) ->
                List.find (\( s, _, _ ) -> s >= Tuple.second offset) l
            )
        |> Maybe.join
        |> Maybe.map (\( _, n, _ ) -> n)
