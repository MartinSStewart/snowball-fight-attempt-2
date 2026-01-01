module Evergreen.V5.EditorPage exposing (..)

import Effect.WebGL
import Evergreen.V5.FontRender
import Evergreen.V5.Id
import Evergreen.V5.Match
import Evergreen.V5.MatchPage
import Evergreen.V5.Point2d
import Evergreen.V5.Shape
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
    | PressedLayer (Evergreen.V5.Id.Id Evergreen.V5.Shape.LayerId)
    | PressedAddLayer
    | PressedDuplicate
    | PressedRemoveLayer (Evergreen.V5.Id.Id Evergreen.V5.Shape.LayerId)
    | TypedColor
        { red : Int
        , green : Int
        , blue : Int
        }
    | PressedSave
    | TypedLoadFromClipboard String
    | PressedMoveLayerUp (Evergreen.V5.Id.Id Evergreen.V5.Shape.LayerId)
    | PressedMoveLayerDown (Evergreen.V5.Id.Id Evergreen.V5.Shape.LayerId)
    | PressedMirrorX


type alias NodeId =
    { pathIndex : Int
    , nodeIndex : Int
    }


type alias EditorState =
    { layers : SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.Shape.LayerId) Evergreen.V5.Shape.Layer
    , currentLayer : Evergreen.V5.Id.Id Evergreen.V5.Shape.LayerId
    , selectedNodes : SeqSet.SeqSet NodeId
    }


type alias Model =
    { mousePosition : Maybe (Evergreen.V5.Point2d.Point2d Pixels.Pixels Evergreen.V5.MatchPage.ScreenCoordinate)
    , mousePositionPrevious : Maybe (Evergreen.V5.Point2d.Point2d Pixels.Pixels Evergreen.V5.MatchPage.ScreenCoordinate)
    , mouseDownAt : Maybe (Evergreen.V5.Point2d.Point2d Length.Meters Evergreen.V5.Match.WorldCoordinate)
    , wheelDownAt : Maybe (Evergreen.V5.Point2d.Point2d Length.Meters Evergreen.V5.Match.WorldCoordinate)
    , cameraPosition : Evergreen.V5.Point2d.Point2d Length.Meters Evergreen.V5.Match.WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , viewportHeight : Length.Length
    , meshCache :
        SeqDict.SeqDict
            (Evergreen.V5.Id.Id Evergreen.V5.Shape.LayerId)
            { pathMesh : Effect.WebGL.Mesh Evergreen.V5.Match.Vertex
            , pathFillMesh : Effect.WebGL.Mesh Evergreen.V5.FontRender.FontVertex
            }
    , placingPoint :
        Maybe
            { index : NodeId
            , position : Evergreen.V5.Point2d.Point2d Length.Meters Evergreen.V5.Match.WorldCoordinate
            }
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend
