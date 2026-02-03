module Evergreen.V8.Route exposing (..)

import Evergreen.V8.Id


type Route
    = HomePageRoute
    | InMatchRoute (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId)
