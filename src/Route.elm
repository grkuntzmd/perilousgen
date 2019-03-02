module Route exposing (Route(..), match)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, fragment, map, parse, s)


type Route
    = Route (Maybe String)


match : Url -> Route
match url =
    parse route url |> Maybe.withDefault (Route Nothing)


route : Parser (Route -> a) a
route =
    s "perilousgen" </> map Route (fragment identity)
