module DungeonTheme exposing (DungeonThemeModel, dungeonTheme, dungeonThemeView, select)

import DungeonMsg exposing (Msg(..))
import Html exposing (Html, a, button, div, i, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, type_)
import Html.Events exposing (onClick)
import Icons exposing (dice)
import List.Extra as List
import Maybe.Extra as Maybe
import Tables exposing (Count(..), TableType(..))


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
    Maybe String
    -> Int
    -> Html Msg
dungeonThemeView field index =
    let
        items =
            List.concat <|
                List.map
                    (\( heading, _, entries ) ->
                        li [ class "uk-nav-header" ] [ text heading ]
                            :: List.map
                                (\( _, n, _ ) ->
                                    li
                                        [ classList [ ( "uk-active", Maybe.unwrap False ((==) n) field ) ] ]
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

        name =
            Maybe.withDefault "none" field
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
            , span [ class "uk-label uk-margin-small-left" ] [ text name ]
            ]
        ]


select : ( Int, Int ) -> Maybe String
select offset =
    List.find (\( _, f, _ ) -> f >= Tuple.first offset) dungeonTheme
        |> Maybe.map
            (\( _, _, l ) ->
                List.find (\( s, _, _ ) -> s >= Tuple.second offset) l
            )
        |> Maybe.join
        |> Maybe.map (\( _, n, _ ) -> n)
