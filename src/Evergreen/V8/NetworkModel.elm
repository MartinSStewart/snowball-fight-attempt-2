module Evergreen.V8.NetworkModel exposing (..)

import Evergreen.V8.Id


type EventId
    = EventId Never


type alias NetworkModel msg model =
    { idCounter : Evergreen.V8.Id.Id EventId
    , localMsgs :
        List
            { id : Evergreen.V8.Id.Id EventId
            , msg : msg
            }
    , serverState : model
    }
