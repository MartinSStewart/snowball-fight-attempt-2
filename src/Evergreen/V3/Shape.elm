module Evergreen.V3.Shape exposing (..)

import Evergreen.V3.Match
import Evergreen.V3.Point2d
import Evergreen.V3.Vector2d
import Length


type LayerId
    = LayerId Never


type alias PathSegment =
    { position : Evergreen.V3.Point2d.Point2d Length.Meters Evergreen.V3.Match.WorldCoordinate
    , handlePrevious : Evergreen.V3.Vector2d.Vector2d Length.Meters Evergreen.V3.Match.WorldCoordinate
    , handleNext : Evergreen.V3.Vector2d.Vector2d Length.Meters Evergreen.V3.Match.WorldCoordinate
    }


type alias Layer =
    { paths : List (List PathSegment)
    , red : Int
    , green : Int
    , blue : Int
    }
