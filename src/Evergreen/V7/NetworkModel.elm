module Evergreen.V7.NetworkModel exposing (..)

import Evergreen.V7.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V7.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V7.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
