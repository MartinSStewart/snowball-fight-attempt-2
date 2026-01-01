module Evergreen.V7.Route exposing (..)

import Evergreen.V7.Id
import Evergreen.V7.MatchPage


type Route
    = HomePageRoute
    | InMatchRoute (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId)
