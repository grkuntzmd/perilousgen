module Dungeon exposing (Model, init, update, view)

import Beast
import DungeonBuilder exposing (dungeonBuilderView)
import DungeonFunction exposing (dungeonFunctionView)
import DungeonMsg exposing (Msg(..))
import DungeonRuination exposing (dungeonRuinationView)
import DungeonSize exposing (dungeonSize, dungeonSizeView)
import DungeonTheme
import Html exposing (Html, a, div, i, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, href)
import Humanoid
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Tables exposing (OffsetPayload(..), TableType(..))


type alias Model =
    { name : String
    , builder : Maybe String
    , function : Maybe String
    , humanoid : Maybe String
    , beast1 : Maybe String
    , beast2 : Maybe String
    , ruination : Maybe String
    , size : Maybe String
    , themes : List (Maybe String)
    , areas : List (Maybe String)
    }


init : Model
init =
    { name = ""
    , builder = Nothing
    , function = Nothing
    , humanoid = Nothing
    , beast1 = Nothing
    , beast2 = Nothing
    , ruination = Nothing
    , size = Nothing
    , themes = []
    , areas = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenRandomMsg tableType ->
            case tableType of
                Beast1 ->
                    ( model
                    , Random.generate (Pair >> OffsetMsg Beast1) (Random.pair (Random.int 1 12) (Random.int 1 12))
                    )

                Beast2 ->
                    ( model
                    , Random.generate (Pair >> OffsetMsg Beast2) (Random.pair (Random.int 1 12) (Random.int 1 12))
                    )

                DungeonBuilder ->
                    ( model, Random.generate (Singleton >> OffsetMsg DungeonBuilder) (Random.int 1 12) )

                DungeonFunction ->
                    ( model, Random.generate (Singleton >> OffsetMsg DungeonFunction) (Random.int 1 12) )

                DungeonRuination ->
                    ( model, Random.generate (Singleton >> OffsetMsg DungeonRuination) (Random.int 1 12) )

                DungeonSize ->
                    ( model
                    , Random.generate (Singleton >> OffsetMsg DungeonSize) (Random.int 1 12)
                    )

                Humanoid ->
                    ( model
                    , Random.generate (Pair >> OffsetMsg Humanoid) (Random.pair (Random.int 1 12) (Random.int 1 12))
                    )

        OffsetMsg tableType payload ->
            case ( tableType, payload ) of
                ( Beast1, Pair offset ) ->
                    ( { model | beast1 = Beast.select offset }, Cmd.none )

                ( Beast2, Pair offset ) ->
                    ( { model | beast2 = Beast.select offset }, Cmd.none )

                ( DungeonBuilder, Singleton offset ) ->
                    ( { model | builder = DungeonBuilder.select offset }, Cmd.none )

                ( DungeonFunction, Singleton offset ) ->
                    ( { model | function = DungeonFunction.select offset }, Cmd.none )

                ( DungeonRuination, Singleton offset ) ->
                    ( { model | ruination = DungeonRuination.select offset }, Cmd.none )

                ( DungeonSize, Singleton offset ) ->
                    let
                        row =
                            DungeonSize.select offset

                        size =
                            Maybe.map .name row

                        themes =
                            Maybe.unwrap [] (\r -> List.repeat r.themeCount Nothing) row

                        areas =
                            Maybe.unwrap [] (\r -> List.repeat r.areaCount Nothing) row
                    in
                    ( { model
                        | size = size
                        , themes = themes
                        , areas = areas
                      }
                    , Cmd.none
                    )

                ( Humanoid, Pair offset ) ->
                    ( { model | humanoid = Humanoid.select offset }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectMsg tableType name ->
            case tableType of
                Beast1 ->
                    ( { model | beast1 = Just name }, Cmd.none )

                Beast2 ->
                    ( { model | beast2 = Just name }, Cmd.none )

                DungeonBuilder ->
                    ( { model
                        | builder = Just name
                        , humanoid = Nothing
                        , beast1 = Nothing
                        , beast2 = Nothing
                      }
                    , Cmd.none
                    )

                DungeonFunction ->
                    ( { model | function = Just name }, Cmd.none )

                DungeonRuination ->
                    ( { model | ruination = Just name }, Cmd.none )

                DungeonSize ->
                    let
                        row =
                            List.find (.name >> (==) name) dungeonSize

                        themes =
                            Maybe.unwrap [] (\r -> List.repeat r.themeCount Nothing) row

                        areas =
                            Maybe.unwrap [] (\r -> List.repeat r.areaCount Nothing) row
                    in
                    ( { model
                        | size = Just name
                        , themes = themes
                        , areas = areas
                      }
                    , Cmd.none
                    )

                Humanoid ->
                    ( { model | humanoid = Just name, beast1 = Nothing, beast2 = Nothing }, Cmd.none )

        -- Areas
        GenAreasMsg start end ->
            ( model, Random.generate OffsetAreasMsg (Random.int start end) )

        OffsetAreasMsg value ->
            ( { model | areas = List.repeat value Nothing }, Cmd.none )

        SelectAreasMsg value ->
            ( { model | areas = List.repeat value Nothing }, Cmd.none )

        -- Themes
        GenThemeMsg index ->
            ( model
            , Random.generate (OffsetThemeMsg index) (Random.pair (Random.int 1 12) (Random.int 1 12))
            )

        GenThemesMsg start end ->
            ( model, Random.generate OffsetThemesMsg (Random.int start end) )

        OffsetThemeMsg index offset ->
            ( { model | themes = List.setAt index (DungeonTheme.select offset) model.themes }, Cmd.none )

        OffsetThemesMsg value ->
            ( { model | themes = List.repeat value Nothing }, Cmd.none )

        SelectThemeMsg index name ->
            ( { model | themes = List.setAt index (Just name) model.themes }, Cmd.none )

        SelectThemesMsg value ->
            ( { model | themes = List.repeat value Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ attribute "uk-grid" "", class "uk-child-width-1-2@s uk-padding-small" ]
        [ div [ class "uk-container uk-container-expand" ]
            [ ul [ attribute "uk-accordion" "" ]
                [ li []
                    [ div [ class "uk-accordion-title" ]
                        [ nav [ class "uk-navbar" ]
                            [ div [ class "uk-navbar-left" ]
                                [ span [ class "uk-text-large" ] [ text "Dungeon" ] ]
                            , div [ class "uk-navbar-right" ]
                                [ ul [ class "uk-navbar-nav" ]
                                    [ li [] [ a [ href "/perilousgen#dungeon" ] [ i [ class "fas fa-file-alt" ] [] ] ]
                                    , li [] [ a [ href "/" ] [ i [ class "fas fa-home" ] [] ] ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "uk-accordion-content" ]
                        (List.concat
                            [ dungeonBuilderView .builder model SelectMsg GenRandomMsg
                            , dungeonFunctionView .function model SelectMsg GenRandomMsg
                            , dungeonRuinationView .ruination model SelectMsg GenRandomMsg
                            , dungeonSizeView model SelectMsg GenRandomMsg
                            ]
                        )
                    ]
                ]
            , div [ class "uk-flex uk-flex-column uk-flex-between uk-flex-stretch uk-margin-small-top" ]
                []
            ]
        ]
