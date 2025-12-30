module Evergreen.V2.Shape exposing (..)

import Evergreen.V2.Match
import Evergreen.V2.Point2d
import Evergreen.V2.Vector2d
import Length


type LayerId
    = LayerId Never


type alias PathSegment =
    { position : Evergreen.V2.Point2d.Point2d Length.Meters Evergreen.V2.Match.WorldCoordinate
    , handlePrevious : Evergreen.V2.Vector2d.Vector2d Length.Meters Evergreen.V2.Match.WorldCoordinate
    , handleNext : Evergreen.V2.Vector2d.Vector2d Length.Meters Evergreen.V2.Match.WorldCoordinate
    }


type alias Layer =
    { paths : List (List PathSegment)
    , red : Int
    , green : Int
    , blue : Int
    }
