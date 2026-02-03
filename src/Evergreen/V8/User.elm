module Evergreen.V8.User exposing (..)


type UserId
    = UserId Never


type alias BackendUser =
    { name : String
    }
