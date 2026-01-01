module Evergreen.V5.Shape exposing (..)

import Evergreen.V5.Match
import Evergreen.V5.Point2d
import Evergreen.V5.Vector2d
import Length


type LayerId
    = LayerId Never


type alias PathSegment =
    { position : Evergreen.V5.Point2d.Point2d Length.Meters Evergreen.V5.Match.WorldCoordinate
    , handlePrevious : Evergreen.V5.Vector2d.Vector2d Length.Meters Evergreen.V5.Match.WorldCoordinate
    , handleNext : Evergreen.V5.Vector2d.Vector2d Length.Meters Evergreen.V5.Match.WorldCoordinate
    }


type alias Layer =
    { paths : List (List PathSegment)
    , red : Int
    , green : Int
    , blue : Int
    }
