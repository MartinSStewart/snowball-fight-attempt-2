module Evergreen.V7.Shape exposing (..)

import Evergreen.V7.Match
import Evergreen.V7.Point2d
import Evergreen.V7.Vector2d
import Length


type LayerId
    = LayerId Never


type alias PathSegment =
    { position : Evergreen.V7.Point2d.Point2d Length.Meters Evergreen.V7.Match.WorldCoordinate
    , handlePrevious : Evergreen.V7.Vector2d.Vector2d Length.Meters Evergreen.V7.Match.WorldCoordinate
    , handleNext : Evergreen.V7.Vector2d.Vector2d Length.Meters Evergreen.V7.Match.WorldCoordinate
    }


type alias Layer =
    { paths : List (List PathSegment)
    , red : Int
    , green : Int
    , blue : Int
    }
