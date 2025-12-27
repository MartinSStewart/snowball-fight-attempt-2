module Evergreen.V1.NetworkModel exposing (..)

import Evergreen.V1.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V1.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V1.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
