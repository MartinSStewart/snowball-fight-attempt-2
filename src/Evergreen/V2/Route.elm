module Evergreen.V2.Route exposing (..)

import Evergreen.V2.Id
import Evergreen.V2.MatchPage


type Route
    = HomePageRoute
    | InMatchRoute (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId)
