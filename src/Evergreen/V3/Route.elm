module Evergreen.V3.Route exposing (..)

import Evergreen.V3.Id
import Evergreen.V3.MatchPage


type Route
    = HomePageRoute
    | InMatchRoute (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId)
