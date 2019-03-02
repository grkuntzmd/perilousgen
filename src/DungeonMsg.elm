module DungeonMsg exposing (Msg(..))

import Tables exposing (OffsetPayload(..), TableType)


type Msg
    = GenRandomMsg TableType
    | OffsetMsg TableType OffsetPayload
    | TableMsg TableType String
