module Evergreen.V3.NetworkModel exposing (..)

import Evergreen.V3.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V3.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V3.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
