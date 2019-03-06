module Dungeon exposing (Model, init, update, view)

import Beast
import DungeonBuilder exposing (dungeonBuilderView)
import DungeonFunction exposing (dungeonFunctionView)
import DungeonMsg exposing (Msg(..))
import DungeonRuination exposing (dungeonRuinationView)
import DungeonSize exposing (dungeonSize, dungeonSizeView)
import DungeonTheme exposing (DungeonThemeTracker, emptyTracker)
import Html exposing (Html, a, button, dd, div, dl, dt, input, li, nav, p, span, text, ul)
import Html.Attributes exposing (attribute, checked, class, href, id, min, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onInput)
import Humanoid
import Icons exposing (file, home)
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Set exposing (Set)
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
    , themes : List DungeonThemeTracker
    , countdowns : Int
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
    , countdowns = 0
    , areas = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        emptyThemes value =
            { model | themes = List.repeat value emptyTracker |> renumberThemeIndexes }

        setThemeName index name =
            { model
                | themes =
                    List.setAt index { start = 1, stop = 1, checked = Set.empty, name = name } model.themes
                        |> renumberThemeIndexes
            }
    in
    case msg of
        GenRandomMsg tableType ->
            updateGenRandom model tableType

        OffsetMsg tableType payload ->
            updateOffset model tableType payload

        SelectMsg tableType name ->
            updateSelect model tableType name

        -- Areas
        GenAreasMsg start end ->
            ( model, Random.generate OffsetAreasMsg (Random.int start end) )

        OffsetAreasMsg value ->
            ( { model | areas = List.repeat value Nothing }, Cmd.none )

        SelectAreasMsg value ->
            ( { model | areas = List.repeat value Nothing }, Cmd.none )

        -- Themes
        CountdownThemeMsg index place checked ->
            ( { model
                | themes =
                    List.updateAt index
                        (\t ->
                            { t
                                | checked =
                                    if checked then
                                        Set.insert place t.checked

                                    else
                                        Set.remove place t.checked
                            }
                        )
                        model.themes
              }
            , Cmd.none
            )

        GenThemeMsg index ->
            ( model
            , Random.generate (OffsetThemeMsg index) (Random.pair (Random.int 1 12) (Random.int 1 12))
            )

        GenThemesMsg start end ->
            ( model, Random.generate OffsetThemesMsg (Random.int start end) )

        OffsetThemeMsg index offset ->
            ( setThemeName index (DungeonTheme.select offset), Cmd.none )

        OffsetThemesMsg value ->
            ( emptyThemes value, Cmd.none )

        SelectThemeMsg index name ->
            ( setThemeName index (Just name), Cmd.none )

        SelectThemesMsg value ->
            ( emptyThemes value, Cmd.none )


renumberThemeIndexes : List DungeonThemeTracker -> List DungeonThemeTracker
renumberThemeIndexes themes =
    List.indexedMap (\i t -> { t | start = i + 1, stop = i + 1 }) themes


updateGenRandom : Model -> TableType -> ( Model, Cmd Msg )
updateGenRandom model tableType =
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


updateOffset : Model -> TableType -> OffsetPayload -> ( Model, Cmd Msg )
updateOffset model tableType payload =
    case ( tableType, payload ) of
        ( Beast1, Pair offset ) ->
            ( { model | beast1 = Beast.select offset }, Cmd.none )

        ( Beast2, Pair offset ) ->
            ( { model | beast2 = Beast.select offset }, Cmd.none )

        ( DungeonBuilder, Singleton offset ) ->
            ( { model
                | builder = DungeonBuilder.select offset
                , humanoid = Nothing
                , beast1 = Nothing
                , beast2 = Nothing
              }
            , Cmd.none
            )

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
                    Maybe.unwrap [] (\r -> List.repeat r.themeCount emptyTracker) row
                        |> renumberThemeIndexes

                areas =
                    Maybe.unwrap [] (\r -> List.repeat r.areaCount Nothing) row
            in
            ( { model
                | size = size
                , themes = themes
                , countdowns = Maybe.unwrap 0 .themeCount row
                , areas = areas
              }
            , Cmd.none
            )

        ( Humanoid, Pair offset ) ->
            ( { model
                | humanoid = Humanoid.select offset
                , beast1 = Nothing
                , beast2 = Nothing
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateSelect : Model -> TableType -> String -> ( Model, Cmd Msg )
updateSelect model tableType name =
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
                    Maybe.unwrap [] (\r -> List.repeat r.themeCount emptyTracker) row
                        |> renumberThemeIndexes

                areas =
                    Maybe.unwrap [] (\r -> List.repeat r.areaCount Nothing) row
            in
            ( { model
                | size = Just name
                , themes = themes
                , countdowns = Maybe.unwrap 0 .themeCount row
                , areas = areas
              }
            , Cmd.none
            )

        Humanoid ->
            ( { model | humanoid = Just name, beast1 = Nothing, beast2 = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "uk-flex uk-flex-top uk-flex-wrap uk-child-width-1-2@m uk-padding-small" ]
        [ div [ class "uk-container uk-container-expand" ]
            [ ul [ attribute "uk-accordion" "" ]
                [ li []
                    [ div [ class "uk-accordion-title" ]
                        [ nav [ class "uk-navbar" ]
                            [ div [ class "uk-navbar-left" ]
                                [ span [ class "uk-text-large" ] [ text "Dungeon Gen" ] ]
                            , div [ class "uk-navbar-right" ]
                                [ ul [ class "uk-navbar-nav" ]
                                    [ li [] [ a [ href "/perilousgen#dungeon" ] [ span [] [ Icons.file ] ] ]
                                    , li [] [ a [ href "/" ] [ span [] [ Icons.home ] ] ]
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
        , summaryView model
        ]


summaryView : Model -> Html Msg
summaryView model =
    let
        builder =
            div [] [ text ("BUILDER: " ++ Maybe.withDefault "none" model.builder) ]
                :: Maybe.unwrap [] (\v -> [ div [] [ text ("HUMANOID: " ++ v) ] ]) model.humanoid
                ++ Maybe.unwrap [] (\v -> [ div [] [ text ("BEAST: " ++ v) ] ]) model.beast1
                ++ Maybe.unwrap [] (\v -> [ div [] [ text ("BEAST: " ++ v) ] ]) model.beast2
    in
    div [ class "uk-flex uk-flex-column uk-flex-between uk-flex-stretch" ]
        [ div [ class "uk-flex uk-flex-between uk-flex-stretch" ]
            [ input [ class "uk-input uk-flex-1", placeholder "Name", type_ "text" ] []
            , span [ attribute "uk-icon" "plus-circle", class "uk-margin-small-left vertically-centered" ] []
            , span [ attribute "uk-icon" "trash", class "uk-margin-small-left vertically-centered" ] []
            ]
        , div [ class "uk-margin-small-top" ] builder
        , div [] [ text ("FUNCTION: " ++ Maybe.withDefault "none" model.function) ]
        , div [] [ text ("SIZE: " ++ Maybe.withDefault "none" model.size) ]
        , div [] [ text ("RUINATION: " ++ Maybe.withDefault "none" model.ruination) ]
        , themesView model
        ]


themesView : Model -> Html Msg
themesView model =
    let
        themeDieSize : List DungeonThemeTracker -> Html Msg
        themeDieSize items =
            let
                max_ =
                    List.foldl (\{ stop } m -> max m stop) 0 items
            in
            if max_ > 0 then
                span [ class "uk-margin-small-left" ] [ text ("(1d" ++ String.fromInt max_ ++ ")") ]

            else
                span [] []

        checkboxes index checked_ =
            List.range 0 (model.countdowns - 1)
                |> List.map
                    (\i ->
                        input
                            [ checked (Set.member i checked_)
                            , class "uk-checkbox uk-margin-small-left"
                            , onCheck (CountdownThemeMsg index i)
                            , type_ "checkbox"
                            ]
                            []
                    )
    in
    dl
        [ class "uk-description-list uk-margin-top" ]
        [ dt []
            [ span [] [ text "THEMES" ]
            , themeDieSize model.themes
            , span
                [ attribute "uk-icon" "info"
                , attribute "uk-toggle" "#theme-dice"
                , class "uk-margin-small-left"
                ]
                []
            , div [ attribute "uk-modal" "", id "theme-dice" ]
                [ div [ class "uk-modal-dialog uk-modal-body" ]
                    [ button
                        [ attribute "uk-close" "", class "uk-modal-close-default", type_ "button" ]
                        []
                    , p [] [ text """If this show a strange die size (like 1d7), just use the next
                    higher size (1d8) and ignore any value above the high value of the weird
                    die.""" ]
                    ]
                ]
            ]
        , dd []
            [ div [ class "dungeon-theme" ]
                (List.indexedMap
                    (\i { start, stop, checked, name } ->
                        [ div []
                            [ span
                                [ style "visibility"
                                    (if start == stop then
                                        "hidden"

                                     else
                                        "visible"
                                    )
                                ]
                                [ text (String.fromInt start ++ " - ") ]
                            , input
                                [ class "uk-input uk-form-width-xsmall"

                                -- , onInput (CountdownThemeMsg index i)
                                , type_ "number"
                                , min "1"
                                , value (String.fromInt stop)
                                ]
                                []
                            ]
                        , div [] [ text (Maybe.withDefault "none" name) ]
                        , div [] (checkboxes i checked)
                        ]
                    )
                    model.themes
                    |> List.concat
                )
            ]
        ]
