module Evergreen.V1.Route exposing (..)

import Evergreen.V1.Id
import Evergreen.V1.MatchPage


type Route
    = HomePageRoute
    | InMatchRoute (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId)
