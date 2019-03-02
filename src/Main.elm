module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Char
import Dungeon
import DungeonMsg
import Html exposing (Html, a, button, div, h4, hr, i, li, p, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, target, type_)
import Parser exposing ((|.), (|=))
import Result exposing (Result(..))
import Route exposing (Route(..), match)
import Set
import Url


type alias Model =
    { dungeon : Dungeon.Model
    , key : Nav.Key
    , route : Route
    }


type Msg
    = DungeonMsg DungeonMsg.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { dungeon = Dungeon.init
      , key = key
      , route = Route Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- Tuple.second <|
    --     Debug.log "Main msg, update"
    --         ( msg
    --         ,
    case msg of
        DungeonMsg msg_ ->
            let
                ( model_, cmd ) =
                    Dungeon.update msg_ model.dungeon
            in
            ( { model | dungeon = model_ }, Cmd.map DungeonMsg cmd )

        LinkClicked url ->
            case url of
                Browser.Internal url_ ->
                    ( model, Nav.pushUrl model.key (Url.toString url_) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = match url }, Cmd.none )



-- )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Fragment
    = LoadDungeon String
    | NewDungeon


view : Model -> Browser.Document Msg
view model =
    let
        parser =
            Parser.oneOf
                [ Parser.succeed NewDungeon
                    |. Parser.keyword "dungeon"
                , Parser.succeed LoadDungeon
                    |. Parser.keyword "dungeon"
                    |. Parser.symbol "/"
                    |= Parser.variable
                        { start = Char.isAlpha
                        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-'
                        , reserved = Set.empty
                        }
                ]

        page =
            case model.route of
                Route (Just value) ->
                    case Parser.run parser value of
                        Ok (LoadDungeon uuid) ->
                            homeView model

                        Ok NewDungeon ->
                            Html.map DungeonMsg (Dungeon.view model.dungeon)

                        Err _ ->
                            homeView model

                Route Nothing ->
                    homeView model
    in
    { title = "Perilous Wilds Generator"
    , body =
        [ div
            [ attribute "uk-navbar" ""
            , class "uk-navbar uk-navbar-container pad-left"
            ]
            [ div [ class "uk-navbar-left" ]
                [ span [ class "uk-text-large" ] [ text "Perilous Gen" ] ]
            , div [ class "uk-navbar-right" ]
                [ ul [ class "uk-navbar-nav" ]
                    [ li []
                        [ a [ attribute "uk-toggle" "#license", href "#" ] [ text "License" ]
                        ]
                    ]
                ]
            , div [ attribute "uk-modal" "", id "license" ]
                [ div [ class "uk-modal-dialog uk-modal-body" ]
                    [ button [ attribute "uk-close" "", class "uk-modal-close-default", type_ "button" ]
                        []
                    , h4 [ class "uk-modal-title" ] [ text "Apache 2.0 License" ]
                    , p [] [ text "Copyright © 2019, G. Ralph Kuntz, MD" ]
                    , p []
                        [ text """Licensed under the Apache License, Version 2.0
                               (the "License"); you may not use this file except
                               in compliance with the License. You may obtain a
                               copy of the License at""" ]
                    , p []
                        [ a
                            [ href "http://www.apache.org/licenses/LICENSE-2.0"
                            , target "_blank"
                            ]
                            [ text "http://www.apache.org/licenses/LICENSE-2.0" ]
                        ]
                    , p []
                        [ text """Unless required by applicable law or agreed
                               to in writing, software distributed under the
                               License is disheadertributed on an "AS IS" BASIS,
                               WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
                               either express or implied. See the License for
                               the specific language governing permissions and
                               limitations under the License.""" ]
                    , p [] [ text "Source code available at " ]
                    , p []
                        [ a
                            [ href "https://github.com/grkuntzmd/perilousgen"
                            , target "_blank"
                            ]
                            [ text "https://github.com/grkuntzmd/perilousgen" ]
                        ]
                    , hr [] []
                    , p []
                        [ text "The contents of this application rely upon the rules contained in "
                        , i [] [ text "Dungeon World" ]
                        , text """, by Adam Koebel and Sage LaTorra, licensed under a Creative Commons
                                Attribution 3.0 Unported license, and """
                        , i [] [ text "The Perilous Wilds" ]
                        , text """, by Jason Lutes and Jeremy Strandberg, licensed under a Creative
                                Commons AttributionShareAlike 3.0 Unported license."""
                        ]
                    ]
                ]
            ]
        , page
        ]
    }


homeView : Model -> Html Msg
homeView _ =
    div [ class "uk-container uk-container-expand uk-padding-small" ]
        [ ul [ class "uk-nav uk-nav-primary uk-nav-center uk-margin-small-auto-vertical" ]
            [ li [] [ a [ href "#" ] [ text "Lead the Way" ] ]
            , li [] [ a [ href "#" ] [ text "See the World" ] ]
            , li [] [ a [ href "#" ] [ text "Weather the Storm" ] ]
            , li [] [ a [ href "#" ] [ text "Ask the Fates" ] ]
            , li [] [ a [ href "/perilousgen#dungeon" ] [ text "Plumb the Depths" ] ]
            , li [] [ a [ href "#" ] [ text "Name Every Person" ] ]
            ]
        ]
