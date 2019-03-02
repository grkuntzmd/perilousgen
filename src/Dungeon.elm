module Dungeon exposing (Model, init, onClickNoProp, update, view)

import Beast
import DungeonBuilder exposing (dungeonBuilderView)
import DungeonFunction exposing (dungeonFunctionView)
import DungeonMsg exposing (Msg(..))
import DungeonRuination exposing (dungeonRuinationView)
import Html exposing (Attribute, Html, a, div, i, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, href)
import Html.Events exposing (stopPropagationOn)
import Humanoid
import Json.Decode as Json
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

                ( Humanoid, Pair offset ) ->
                    ( { model | humanoid = Humanoid.select offset }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TableMsg tableType name ->
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

                Humanoid ->
                    ( { model | humanoid = Just name, beast1 = Nothing, beast2 = Nothing }, Cmd.none )


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
                                    [ li []
                                        [ a
                                            [ href "#"

                                            {- , onClickNoProp GenDungeon -}
                                            ]
                                            [ i [ class "fas fa-dice" ] [] ]
                                        ]
                                    , li [] [ a [ href "/perilousgen#dungeon" ] [ i [ class "fas fa-file-alt" ] [] ] ]
                                    , li [] [ a [ href "/" ] [ i [ class "fas fa-home" ] [] ] ]
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "uk-accordion-content" ]
                        (List.concat
                            [ dungeonBuilderView .builder model TableMsg GenRandomMsg
                            , dungeonFunctionView .function model TableMsg GenRandomMsg
                            , dungeonRuinationView .ruination model TableMsg GenRandomMsg
                            ]
                        )
                    ]
                ]
            , div [ class "uk-flex uk-flex-column uk-flex-between uk-flex-stretch uk-margin-small-top" ]
                []
            ]
        ]


onClickNoProp : msg -> Attribute msg
onClickNoProp msg =
    stopPropagationOn "click" (Json.map (\m -> ( m, True )) (Json.succeed msg))
