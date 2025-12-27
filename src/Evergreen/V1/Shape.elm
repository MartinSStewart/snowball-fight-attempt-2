module Evergreen.V1.Shape exposing (..)

import Evergreen.V1.Match
import Evergreen.V1.Point2d
import Evergreen.V1.Vector2d
import Length


type LayerId
    = LayerId Never


type alias PathSegment =
    { position : Evergreen.V1.Point2d.Point2d Length.Meters Evergreen.V1.Match.WorldCoordinate
    , handlePrevious : Evergreen.V1.Vector2d.Vector2d Length.Meters Evergreen.V1.Match.WorldCoordinate
    , handleNext : Evergreen.V1.Vector2d.Vector2d Length.Meters Evergreen.V1.Match.WorldCoordinate
    }


type alias Layer =
    { paths : List (List PathSegment)
    , red : Int
    , green : Int
    , blue : Int
    }
