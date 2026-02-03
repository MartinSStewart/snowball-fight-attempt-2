module Evergreen.V8.Shape exposing (..)

import Evergreen.V8.Match
import Evergreen.V8.Point2d
import Evergreen.V8.Vector2d
import Length


type LayerId
    = LayerId Never


type alias PathSegment =
    { position : Evergreen.V8.Point2d.Point2d Length.Meters Evergreen.V8.Match.WorldCoordinate
    , handlePrevious : Evergreen.V8.Vector2d.Vector2d Length.Meters Evergreen.V8.Match.WorldCoordinate
    , handleNext : Evergreen.V8.Vector2d.Vector2d Length.Meters Evergreen.V8.Match.WorldCoordinate
    }


type alias Layer =
    { paths : List (List PathSegment)
    , red : Int
    , green : Int
    , blue : Int
    }
