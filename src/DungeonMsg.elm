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
    | GenCountdownsMsg Int Int
    | OffsetCountdownsMsg Int
    | SelectCountdownsMsg Int
    | GenThemesMsg Int Int
    | OffsetThemesMsg Int
    | SelectThemesMsg Int
      -- Themes
    | CountdownThemeMsg Int Int Bool
    | GenRandomElementMsg Int TableType
    | OffsetElementMsg Int Int
    | OffsetThemeMsg Int ( Int, Int )
    | SelectElementMsg Int TableType String
    | SelectThemeMsg Int String
    | StopThemeMsg Int String
