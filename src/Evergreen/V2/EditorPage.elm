module Evergreen.V2.EditorPage exposing (..)

import Effect.WebGL
import Evergreen.V2.FontRender
import Evergreen.V2.Id
import Evergreen.V2.Match
import Evergreen.V2.MatchPage
import Evergreen.V2.Point2d
import Evergreen.V2.Shape
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
    | PressedLayer (Evergreen.V2.Id.Id Evergreen.V2.Shape.LayerId)
    | PressedAddLayer
    | PressedDuplicate
    | PressedRemoveLayer (Evergreen.V2.Id.Id Evergreen.V2.Shape.LayerId)
    | TypedColor
        { red : Int
        , green : Int
        , blue : Int
        }
    | PressedSave
    | TypedLoadFromClipboard String
    | PressedMoveLayerUp (Evergreen.V2.Id.Id Evergreen.V2.Shape.LayerId)
    | PressedMoveLayerDown (Evergreen.V2.Id.Id Evergreen.V2.Shape.LayerId)
    | PressedMirrorX


type alias NodeId =
    { pathIndex : Int
    , nodeIndex : Int
    }


type alias EditorState =
    { layers : SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.Shape.LayerId) Evergreen.V2.Shape.Layer
    , currentLayer : Evergreen.V2.Id.Id Evergreen.V2.Shape.LayerId
    , selectedNodes : SeqSet.SeqSet NodeId
    }


type alias Model =
    { mousePosition : Maybe (Evergreen.V2.Point2d.Point2d Pixels.Pixels Evergreen.V2.MatchPage.ScreenCoordinate)
    , mousePositionPrevious : Maybe (Evergreen.V2.Point2d.Point2d Pixels.Pixels Evergreen.V2.MatchPage.ScreenCoordinate)
    , mouseDownAt : Maybe (Evergreen.V2.Point2d.Point2d Length.Meters Evergreen.V2.Match.WorldCoordinate)
    , wheelDownAt : Maybe (Evergreen.V2.Point2d.Point2d Length.Meters Evergreen.V2.Match.WorldCoordinate)
    , cameraPosition : Evergreen.V2.Point2d.Point2d Length.Meters Evergreen.V2.Match.WorldCoordinate
    , editorState : EditorState
    , undoHistory : List EditorState
    , redoHistory : List EditorState
    , viewportHeight : Length.Length
    , meshCache :
        SeqDict.SeqDict
            (Evergreen.V2.Id.Id Evergreen.V2.Shape.LayerId)
            { pathMesh : Effect.WebGL.Mesh Evergreen.V2.Match.Vertex
            , pathFillMesh : Effect.WebGL.Mesh Evergreen.V2.FontRender.FontVertex
            }
    , placingPoint :
        Maybe
            { index : NodeId
            , position : Evergreen.V2.Point2d.Point2d Length.Meters Evergreen.V2.Match.WorldCoordinate
            }
    }


type ToBackend
    = NoOpToBackend


type ToFrontend
    = NoOpToFrontend
