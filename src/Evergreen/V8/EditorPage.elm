module Evergreen.V8.EditorPage exposing (..)

import Effect.WebGL
import Evergreen.V8.FontRender
import Evergreen.V8.Id
import Evergreen.V8.Match
import Evergreen.V8.MatchPage
import Evergreen.V8.Point2d
import Evergreen.V8.Shape
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
    | PressedLayer (Evergreen.V8.Id.Id Evergreen.V8.Shape.LayerId)
    | PressedAddLayer
    | PressedDuplicate
    | PressedRemoveLayer (Evergreen.V8.Id.Id Evergreen.V8.Shape.LayerId)
    | TypedColor
        { red : Int
        , green : Int
        , blue : Int
        }
    | PressedSave
    | TypedLoadFromClipboard String
    | PressedMoveLayerUp (Evergreen.V8.Id.Id Evergreen.V8.Shape.LayerId)
    | PressedMoveLayerDown (Evergreen.V8.Id.Id Evergreen.V8.Shape.LayerId)
    | PressedMirrorX


type alias NodeId =
    { pathIndex : Int
    , nodeIndex : Int
    }


type alias EditorState =
    { layers : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.Shape.LayerId) Evergreen.V8.Shape.Layer
    , currentLayer : Evergreen.V8.Id.Id Evergreen.V8.Shape.LayerId
    , selectedNodes : SeqSet.SeqSet NodeId
    }


type alias Model =
    { mousePosition : Maybe (Evergreen.V8.Point2d.Point2d Pixels.Pixels Evergreen.V8.MatchPage.ScreenCoordinate)
    , mousePositionPrevious : Maybe (Evergreen.V8.Point2d.Point2d Pixels.Pixels Evergreen.V8.MatchPage.ScreenCoordinate)
    , mouseDownAt : Maybe (Evergreen.V8.Point2d.Point2d Length.Meters Evergreen.V8.Match.WorldCoordinate)
    , wheelDownAt : Maybe (Evergreen.V8.Point2d.Point2d Length.Meters Evergreen.V8.Match.WorldCoordinate)
    , cameraPosition : Evergreen.V8.Point2d.Point2d Length.Meters Evergreen.V8.Match.WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , viewportHeight : Length.Length
    , meshCache :
        SeqDict.SeqDict
            (Evergreen.V8.Id.Id Evergreen.V8.Shape.LayerId)
            { pathMesh : Effect.WebGL.Mesh Evergreen.V8.Match.Vertex
            , pathFillMesh : Effect.WebGL.Mesh Evergreen.V8.FontRender.FontVertex
            }
    , placingPoint :
        Maybe
            { index : NodeId
            , position : Evergreen.V8.Point2d.Point2d Length.Meters Evergreen.V8.Match.WorldCoordinate
            }
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend
