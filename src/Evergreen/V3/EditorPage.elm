module Evergreen.V3.EditorPage exposing (..)

import Effect.WebGL
import Evergreen.V3.FontRender
import Evergreen.V3.Id
import Evergreen.V3.Match
import Evergreen.V3.MatchPage
import Evergreen.V3.Point2d
import Evergreen.V3.Shape
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
    | PressedLayer (Evergreen.V3.Id.Id Evergreen.V3.Shape.LayerId)
    | PressedAddLayer
    | PressedDuplicate
    | PressedRemoveLayer (Evergreen.V3.Id.Id Evergreen.V3.Shape.LayerId)
    | TypedColor
        { red : Int
        , green : Int
        , blue : Int
        }
    | PressedSave
    | TypedLoadFromClipboard String
    | PressedMoveLayerUp (Evergreen.V3.Id.Id Evergreen.V3.Shape.LayerId)
    | PressedMoveLayerDown (Evergreen.V3.Id.Id Evergreen.V3.Shape.LayerId)
    | PressedMirrorX


type alias NodeId =
    { pathIndex : Int
    , nodeIndex : Int
    }


type alias EditorState =
    { layers : SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.Shape.LayerId) Evergreen.V3.Shape.Layer
    , currentLayer : Evergreen.V3.Id.Id Evergreen.V3.Shape.LayerId
    , selectedNodes : SeqSet.SeqSet NodeId
    }


type alias Model =
    { mousePosition : Maybe (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.MatchPage.ScreenCoordinate)
    , mousePositionPrevious : Maybe (Evergreen.V3.Point2d.Point2d Pixels.Pixels Evergreen.V3.MatchPage.ScreenCoordinate)
    , mouseDownAt : Maybe (Evergreen.V3.Point2d.Point2d Length.Meters Evergreen.V3.Match.WorldCoordinate)
    , wheelDownAt : Maybe (Evergreen.V3.Point2d.Point2d Length.Meters Evergreen.V3.Match.WorldCoordinate)
    , cameraPosition : Evergreen.V3.Point2d.Point2d Length.Meters Evergreen.V3.Match.WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , viewportHeight : Length.Length
    , meshCache :
        SeqDict.SeqDict
            (Evergreen.V3.Id.Id Evergreen.V3.Shape.LayerId)
            { pathMesh : Effect.WebGL.Mesh Evergreen.V3.Match.Vertex
            , pathFillMesh : Effect.WebGL.Mesh Evergreen.V3.FontRender.FontVertex
            }
    , placingPoint :
        Maybe
            { index : NodeId
            , position : Evergreen.V3.Point2d.Point2d Length.Meters Evergreen.V3.Match.WorldCoordinate
            }
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend
