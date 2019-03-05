module Tables exposing
    ( Count(..)
    , GenRandomMsg
    , OffsetPayload(..)
    , SelectMsg
    , TableType(..)
    , complexView
    , simpleView
    )

import Html exposing (Html, a, button, div, i, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, type_)
import Html.Events exposing (onClick)
import Icons exposing (dice)
import List.Extra as List
import Maybe.Extra as Maybe


type Count a
    = Double a a
    | Single a


type TableType
    = Beast1
    | Beast2
    | DungeonBuilder
    | DungeonFunction
    | DungeonRuination
    | DungeonSize
    | Humanoid


type OffsetPayload
    = Pair ( Int, Int )
    | Singleton Int


type alias GenRandomMsg msg =
    TableType -> msg


type alias SelectMsg msg =
    TableType -> String -> msg


complexView :
    (m -> Maybe String)
    -> m
    -> SelectMsg msg
    -> GenRandomMsg msg
    -> String
    -> TableType
    ->
        List
            ( String
            , Int
            , List
                ( Int
                , String
                , Maybe
                    (Count
                        (m
                         -> SelectMsg msg
                         -> GenRandomMsg msg
                         -> List (Html msg)
                        )
                    )
                )
            )
    -> List (Html msg)
complexView field model selectMsg genMsg title tableType table =
    let
        model_ =
            field model

        entry =
            List.find (\( _, n, _ ) -> Just n == model_) (List.concatMap (\( _, _, l ) -> l) table)

        items =
            List.concat <|
                List.map
                    (\( heading, _, entries ) ->
                        li [ class "uk-nav-header" ] [ text heading ]
                            :: List.map
                                (\( _, n, _ ) ->
                                    li
                                        [ classList [ ( "uk-active", Maybe.unwrap False ((==) n) model_ ) ] ]
                                        [ a
                                            [ href "/perilousgen#dungeon"
                                            , onClick (selectMsg tableType n)
                                            ]
                                            [ text n ]
                                        ]
                                )
                                entries
                    )
                    table

        name =
            Maybe.withDefault "none" model_
    in
    li []
        [ div [ class "uk-inline" ]
            [ span [ onClick (genMsg tableType) ] [ Icons.dice ]
            , button
                [ class "uk-button uk-button-default uk-margin-small-left uk-button-text", type_ "button" ]
                [ text title ]
            , div
                [ attribute "uk-dropdown" "mode: click", class "uk-dropdown" ]
                [ ul [ class "uk-nav uk-dropdown-nav" ] items ]
            , span [ class "uk-label uk-margin-small-left" ] [ text name ]
            ]
        ]
        :: (case entry of
                Just ( _, _, Just (Single t) ) ->
                    t model selectMsg genMsg

                Just ( _, _, Just (Double t1 t2) ) ->
                    List.append (t1 model selectMsg genMsg) (t2 model selectMsg genMsg)

                _ ->
                    []
           )


simpleView :
    (m -> Maybe String)
    -> m
    -> SelectMsg msg
    -> GenRandomMsg msg
    -> String
    -> TableType
    -> List ( Int, String, Maybe (Count (m -> SelectMsg msg -> GenRandomMsg msg -> List (Html msg))) )
    -> List (Html msg)
simpleView field model selectMsg genMsg title tableType table =
    let
        model_ =
            field model

        entry =
            List.find (\( _, n, _ ) -> Just n == model_) table

        items =
            List.map
                (\( _, n, _ ) ->
                    li
                        [ classList [ ( "uk-active", Maybe.unwrap False ((==) n) model_ ) ] ]
                        [ a
                            [ href "/perilousgen#dungeon"
                            , onClick (selectMsg tableType n)
                            ]
                            [ text n ]
                        ]
                )
                table

        name =
            Maybe.withDefault "none" model_
    in
    li []
        [ div [ class "uk-inline" ]
            [ span [ onClick (genMsg tableType) ] [ Icons.dice ]
            , button
                [ class "uk-button uk-button-default uk-margin-small-left uk-button-text", type_ "button" ]
                [ text title ]
            , div
                [ attribute "uk-dropdown" "mode: click", class "uk-dropdown" ]
                [ ul [ class "uk-nav uk-dropdown-nav" ] items ]
            , span [ class "uk-label uk-margin-small-left" ] [ text name ]
            ]
        ]
        :: (case entry of
                Just ( _, _, Just (Single t) ) ->
                    t model selectMsg genMsg

                Just ( _, _, Just (Double t1 t2) ) ->
                    List.append (t1 model selectMsg genMsg) (t2 model selectMsg genMsg)

                _ ->
                    []
           )
