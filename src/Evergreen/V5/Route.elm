module Evergreen.V5.Route exposing (..)

import Evergreen.V5.Id
import Evergreen.V5.MatchPage


type Route
    = HomePageRoute
    | InMatchRoute (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId)
