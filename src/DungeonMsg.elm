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
    | OffsetThemesMsg Int
    | SelectThemesMsg Int
      -- Themes
    | CountdownThemeMsg Int Int Bool
    | OffsetThemeMsg Int ( Int, Int )
    | SelectThemeMsg Int String
