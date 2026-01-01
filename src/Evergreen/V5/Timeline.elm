module Evergreen.V5.Timeline exposing (..)

import Evergreen.V5.Id
import List.Nonempty
import SeqSet


type FrameId
    = FrameId Never


type alias Timeline input =
    SeqSet.SeqSet ( Evergreen.V5.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List.Nonempty.Nonempty ( Evergreen.V5.Id.Id FrameId, state )
    }
