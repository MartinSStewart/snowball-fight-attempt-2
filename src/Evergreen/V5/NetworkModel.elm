module Evergreen.V5.NetworkModel exposing (..)

import Evergreen.V5.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V5.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V5.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
