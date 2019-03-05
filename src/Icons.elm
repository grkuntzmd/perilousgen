module Icons exposing (dice, file, home)

import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, height, strokeWidth, version, viewBox, width)


dice : Html msg
dice =
    svg [ height "24", version "1.1", viewBox "0 0 30 24", width "30" ]
        [ path
            [ d """m27.75 9h-5.5659c0.59485 1.387 0.33375 3.0562-0.79687 4.1868l-6.3871 6.3871v2.1759c0 1.2427 1.0073
                2.25 2.25 2.25h10.5c1.2427 0 2.25-1.0073 2.25-2.25v-10.5c0-1.2427-1.0073-2.25-2.25-2.25zm-5.25
                8.625c-0.62109 0-1.125-0.50391-1.125-1.125 0-0.62157 0.50391-1.125 1.125-1.125 0.62109 0 1.125 0.50343
                1.125 1.125 0 0.62109-0.50391 1.125-1.125
                1.125zm-2.1736-8.7516-8.1999-8.1999c-0.89812-0.89812-2.3545-0.89812-3.2526 0l-8.2003 8.1999c-0.89812
                0.89812-0.89812 2.3545 0 3.2526l8.1999 8.2003c0.89812 0.89812 2.3545 0.89812 3.2526
                0l8.2003-8.1999c0.89812-0.8986 0.89812-2.355 0-3.2532zm-15.826 2.7516c-0.62109
                0-1.125-0.50391-1.125-1.125 0-0.62157 0.50391-1.125 1.125-1.125s1.125 0.50343 1.125 1.125c0
                0.62109-0.50391 1.125-1.125 1.125zm6 6c-0.62109 0-1.125-0.50391-1.125-1.125 0-0.62157 0.50391-1.125
                1.125-1.125 0.62109 0 1.125 0.50343 1.125 1.125 0 0.62109-0.50391 1.125-1.125 1.125zm0-6c-0.62109
                0-1.125-0.50391-1.125-1.125 0-0.62157 0.50391-1.125 1.125-1.125 0.62109 0 1.125 0.50343 1.125 1.125 0
                0.62109-0.50391 1.125-1.125 1.125zm0-6c-0.62109 0-1.125-0.50391-1.125-1.125 0-0.62157 0.50391-1.125
                1.125-1.125 0.62109 0 1.125 0.50343 1.125 1.125 0 0.62109-0.50391 1.125-1.125 1.125zm6 6c-0.62109
                0-1.125-0.50391-1.125-1.125 0-0.62157 0.50391-1.125 1.125-1.125 0.62109 0 1.125 0.50343 1.125 1.125 0
                0.62109-0.50391 1.125-1.125 1.125z"""
            , fill "currentColor"
            , strokeWidth ".046875"
            ]
            []
        ]


file : Html msg
file =
    svg
        [ height "24", version "1.1", viewBox "0 0 18 24", width "18" ]
        [ path
            [ d """m10.5 6.375v-6.375h-9.375c-0.62344 0-1.125 0.50156-1.125 1.125v21.75c0 0.62344 0.50156 1.125 1.125
                1.125h15.75c0.62344 0 1.125-0.50156 1.125-1.125v-15.375h-6.375c-0.61875 0-1.125-0.50625-1.125-1.125zm3
                11.062c0 0.30938-0.25312 0.5625-0.5625 0.5625h-7.875c-0.30938
                0-0.5625-0.25312-0.5625-0.5625v-0.375c0-0.30938 0.25312-0.5625 0.5625-0.5625h7.875c0.30938 0 0.5625
                0.25312 0.5625 0.5625zm0-3c0 0.30938-0.25312 0.5625-0.5625 0.5625h-7.875c-0.30938
                0-0.5625-0.25312-0.5625-0.5625v-0.375c0-0.30938 0.25312-0.5625 0.5625-0.5625h7.875c0.30938 0 0.5625
                0.25312 0.5625 0.5625zm0-3.375v0.375c0 0.30938-0.25312 0.5625-0.5625 0.5625h-7.875c-0.30938
                0-0.5625-0.25312-0.5625-0.5625v-0.375c0-0.30938 0.25312-0.5625 0.5625-0.5625h7.875c0.30938 0 0.5625
                0.25312 0.5625 0.5625zm4.5-5.3484v0.28594h-6v-6h0.28594c0.3 0 0.58594 0.11719 0.79687 0.32813l4.5891
                4.5938c0.21093 0.21093 0.32812 0.49687 0.32812 0.79218z"""
            , fill "currentColor"
            , strokeWidth ".046875"
            ]
            []
        ]


home : Html msg
home =
    svg
        [ height "24", version "1.1", viewBox "0 0 30.863 24", width "30.863" ]
        [ path
            [ d """m15.022 6.2262-9.8781 8.1357v8.7808a0.85724 0.85724 0 0 0 0.85724 0.85724l6.0039-0.0155a0.85724
                0.85724 0 0 0 0.85296-0.85724v-5.1279a0.85724 0.85724 0 0 1 0.85723-0.85724h3.429a0.85724 0.85724 0 0 1
                0.85724 0.85724v5.1242a0.85724 0.85724 0 0 0 0.85724 0.85991l6.0018 0.0166a0.85724 0.85724 0 0 0
                0.85724-0.85724v-8.7867l-9.8759-8.1298a0.65311 0.65311 0 0 0-0.81973 0zm15.603
                5.5297-4.4791-3.692v-7.421a0.64293 0.64293 0 0 0-0.64293-0.64293h-3.0003a0.64293 0.64293 0 0 0-0.64293
                0.64293v3.8903l-4.7968-3.9465a2.5717 2.5717 0 0 0-3.2682 0l-13.562 11.169a0.64293 0.64293 0 0 0-0.0857
                0.90546l1.3662 1.6609a0.64293 0.64293 0 0 0 0.906 0.0873l12.602-10.38a0.65311 0.65311 0 0 1 0.81973
                0l12.603 10.38a0.64293 0.64293 0 0 0 0.90546-0.0857l1.3662-1.6609a0.64293 0.64293 0 0
                0-0.0911-0.90707z"""
            , fill "currentColor"
            , strokeWidth ".053577"
            ]
            []
        ]