module Evergreen.V7.EditorPage exposing (..)

import Effect.WebGL
import Evergreen.V7.FontRender
import Evergreen.V7.Id
import Evergreen.V7.Match
import Evergreen.V7.MatchPage
import Evergreen.V7.Point2d
import Evergreen.V7.Shape
import Html.Events.Extra.Mouse
import Html.Events.Extra.Wheel
import Length
import Pixels
import SeqDict
import SeqSet


type Msg
    = MouseDown Html.Events.Extra.Mouse.Event
    | MouseUp Html.Events.Extra.Mouse.Event
    | MouseMoved Html.Events.Extra.Mouse.Event
    | MouseLeft Html.Events.Extra.Mouse.Event
    | MouseWheel Html.Events.Extra.Wheel.Event
    | PressedLayer (Evergreen.V7.Id.Id Evergreen.V7.Shape.LayerId)
    | PressedAddLayer
    | PressedDuplicate
    | PressedRemoveLayer (Evergreen.V7.Id.Id Evergreen.V7.Shape.LayerId)
    | TypedColor
        { red : Int
        , green : Int
        , blue : Int
        }
    | PressedSave
    | TypedLoadFromClipboard String
    | PressedMoveLayerUp (Evergreen.V7.Id.Id Evergreen.V7.Shape.LayerId)
    | PressedMoveLayerDown (Evergreen.V7.Id.Id Evergreen.V7.Shape.LayerId)
    | PressedMirrorX


type alias NodeId =
    { pathIndex : Int
    , nodeIndex : Int
    }


type alias EditorState =
    { layers : SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.Shape.LayerId) Evergreen.V7.Shape.Layer
    , currentLayer : Evergreen.V7.Id.Id Evergreen.V7.Shape.LayerId
    , selectedNodes : SeqSet.SeqSet NodeId
    }


type alias Model =
    { mousePosition : Maybe (Evergreen.V7.Point2d.Point2d Pixels.Pixels Evergreen.V7.MatchPage.ScreenCoordinate)
    , mousePositionPrevious : Maybe (Evergreen.V7.Point2d.Point2d Pixels.Pixels Evergreen.V7.MatchPage.ScreenCoordinate)
    , mouseDownAt : Maybe (Evergreen.V7.Point2d.Point2d Length.Meters Evergreen.V7.Match.WorldCoordinate)
    , wheelDownAt : Maybe (Evergreen.V7.Point2d.Point2d Length.Meters Evergreen.V7.Match.WorldCoordinate)
    , cameraPosition : Evergreen.V7.Point2d.Point2d Length.Meters Evergreen.V7.Match.WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , viewportHeight : Length.Length
    , meshCache :
        SeqDict.SeqDict
            (Evergreen.V7.Id.Id Evergreen.V7.Shape.LayerId)
            { pathMesh : Effect.WebGL.Mesh Evergreen.V7.Match.Vertex
            , pathFillMesh : Effect.WebGL.Mesh Evergreen.V7.FontRender.FontVertex
            }
    , placingPoint :
        Maybe
            { index : NodeId
            , position : Evergreen.V7.Point2d.Point2d Length.Meters Evergreen.V7.Match.WorldCoordinate
            }
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend
