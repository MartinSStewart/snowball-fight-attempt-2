module Evergreen.V2.NetworkModel exposing (..)

import Evergreen.V2.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V2.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V2.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
