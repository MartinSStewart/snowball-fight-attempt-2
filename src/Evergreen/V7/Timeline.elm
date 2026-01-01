module Evergreen.V7.Timeline exposing (..)

import Evergreen.V7.Id
import List.Nonempty
import SeqSet


type FrameId
    = FrameId Never


type alias Timeline input =
    SeqSet.SeqSet ( Evergreen.V7.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List.Nonempty.Nonempty ( Evergreen.V7.Id.Id FrameId, state )
    }
