module Evergreen.V3.Timeline exposing (..)

import Evergreen.V3.Id
import List.Nonempty
import SeqSet


type FrameId
    = FrameId Never


type alias Timeline input =
    SeqSet.SeqSet ( Evergreen.V3.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List.Nonempty.Nonempty ( Evergreen.V3.Id.Id FrameId, state )
    }
