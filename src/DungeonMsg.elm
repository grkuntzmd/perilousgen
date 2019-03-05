module DungeonMsg exposing (Msg(..))

import Tables exposing (OffsetPayload(..), TableType)


type Msg
    = GenRandomMsg TableType
    | OffsetMsg TableType OffsetPayload
    | SelectMsg TableType String
      -- Size-based messages
    | GenAreasMsg
    | GenThemesMsg
    | SelectAreasMsg Int
    | SelectThemesMsg Int
