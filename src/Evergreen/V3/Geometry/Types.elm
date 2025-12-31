module Evergreen.V3.Geometry.Types exposing (..)


type Point2d units coordinates
    = Point2d
        { x : Float
        , y : Float
        }


type Vector2d units coordinates
    = Vector2d
        { x : Float
        , y : Float
        }


type Direction2d coordinates
    = Direction2d
        { x : Float
        , y : Float
        }


type Vector3d units coordinates
    = Vector3d
        { x : Float
        , y : Float
        , z : Float
        }


type Point3d units coordinates
    = Point3d
        { x : Float
        , y : Float
        , z : Float
        }
