module Evergreen.V1.EditorPage exposing (..)

import Effect.WebGL
import Evergreen.V1.FontRender
import Evergreen.V1.Id
import Evergreen.V1.Match
import Evergreen.V1.MatchPage
import Evergreen.V1.Point2d
import Evergreen.V1.Shape
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
    | PressedLayer (Evergreen.V1.Id.Id Evergreen.V1.Shape.LayerId)
    | PressedAddLayer
    | PressedDuplicate
    | PressedRemoveLayer (Evergreen.V1.Id.Id Evergreen.V1.Shape.LayerId)
    | TypedColor
        { red : Int
        , green : Int
        , blue : Int
        }
    | PressedSave
    | TypedLoadFromClipboard String
    | PressedMoveLayerUp (Evergreen.V1.Id.Id Evergreen.V1.Shape.LayerId)
    | PressedMoveLayerDown (Evergreen.V1.Id.Id Evergreen.V1.Shape.LayerId)
    | PressedMirrorX


type alias NodeId =
    { pathIndex : Int
    , nodeIndex : Int
    }


type alias EditorState =
    { layers : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.Shape.LayerId) Evergreen.V1.Shape.Layer
    , currentLayer : Evergreen.V1.Id.Id Evergreen.V1.Shape.LayerId
    , selectedNodes : SeqSet.SeqSet NodeId
    }


type alias Model =
    { mousePosition : Maybe (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.MatchPage.ScreenCoordinate)
    , mousePositionPrevious : Maybe (Evergreen.V1.Point2d.Point2d Pixels.Pixels Evergreen.V1.MatchPage.ScreenCoordinate)
    , mouseDownAt : Maybe (Evergreen.V1.Point2d.Point2d Length.Meters Evergreen.V1.Match.WorldCoordinate)
    , wheelDownAt : Maybe (Evergreen.V1.Point2d.Point2d Length.Meters Evergreen.V1.Match.WorldCoordinate)
    , cameraPosition : Evergreen.V1.Point2d.Point2d Length.Meters Evergreen.V1.Match.WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , viewportHeight : Length.Length
    , meshCache :
        SeqDict.SeqDict
            (Evergreen.V1.Id.Id Evergreen.V1.Shape.LayerId)
            { pathMesh : Effect.WebGL.Mesh Evergreen.V1.Match.Vertex
            , pathFillMesh : Effect.WebGL.Mesh Evergreen.V1.FontRender.FontVertex
            }
    , placingPoint :
        Maybe
            { index : NodeId
            , position : Evergreen.V1.Point2d.Point2d Length.Meters Evergreen.V1.Match.WorldCoordinate
            }
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend
