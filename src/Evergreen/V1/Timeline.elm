module Evergreen.V1.Timeline exposing (..)

import Evergreen.V1.Id
import List.Nonempty
import SeqSet


type FrameId
    = FrameId Never


type alias Timeline input =
    SeqSet.SeqSet ( Evergreen.V1.Id.Id FrameId, input )


type Error
    = InputTooOld


type alias TimelineCache state =
    { cache : List.Nonempty.Nonempty ( Evergreen.V1.Id.Id FrameId, state )
    }
