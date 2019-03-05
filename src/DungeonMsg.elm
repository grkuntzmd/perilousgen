module DungeonMsg exposing (Msg(..))

import Tables exposing (OffsetPayload(..), TableType)


type Msg
    = GenRandomMsg TableType
    | OffsetMsg TableType OffsetPayload
    | SelectMsg TableType String
      -- Size-areas
    | GenAreasMsg Int Int
    | OffsetAreasMsg Int
    | SelectAreasMsg Int
      -- Size-themes
    | GenThemeMsg Int
    | GenThemesMsg Int Int
    | OffsetThemeMsg Int ( Int, Int )
    | OffsetThemesMsg Int
    | SelectThemeMsg Int String
    | SelectThemesMsg Int
