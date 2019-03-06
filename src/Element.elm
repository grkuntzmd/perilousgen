module Element exposing (ElementModel, elementView, select)

import Html exposing (Html)
import List.Extra as List
import Tables exposing (GenRandomMsg, SelectMsg, TableType(..), simpleView)


type alias ElementModel m =
    { m | element : Maybe String }


element : List ( Int, String, Maybe x )
element =
    [ ( 2, "air", Nothing )
    , ( 4, "earth", Nothing )
    , ( 6, "fire", Nothing )
    , ( 8, "water", Nothing )
    , ( 10, "life", Nothing )
    , ( 12, "death", Nothing )
    ]


elementView :
    (ElementModel m -> Maybe String)
    -> ElementModel m
    -> SelectMsg msg
    -> GenRandomMsg msg
    -> List (Html msg)
elementView field model selectMsg genMsg =
    simpleView field model selectMsg genMsg "Element" Element element


select : Int -> Maybe String
select offset =
    Maybe.map (\( _, n, _ ) -> n) (List.find (\( i, _, _ ) -> i >= offset) element)
