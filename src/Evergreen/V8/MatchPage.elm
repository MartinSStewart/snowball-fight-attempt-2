module Evergreen.V8.MatchPage exposing (..)

import Effect.Time
import Effect.WebGL
import Evergreen.V8.Character
import Evergreen.V8.Id
import Evergreen.V8.Match
import Evergreen.V8.MatchName
import Evergreen.V8.NetworkModel
import Evergreen.V8.NonemptySet
import Evergreen.V8.Point2d
import Evergreen.V8.TextMessage
import Evergreen.V8.Timeline
import Evergreen.V8.User
import Html.Events.Extra.Pointer
import Length
import Pixels
import SeqDict


type WorldPixel
    = WorldPixel Never


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedCharacter Evergreen.V8.Character.Character
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V8.Match.PlayerMode
    | PressedSaveMatchName Evergreen.V8.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V8.TextMessage.TextMessage
    | TypedMaxPlayers String
    | PressedSaveMaxPlayers Int
    | PressedResetMaxPlayers
    | ScrolledToBottom
    | PointerDown Html.Events.Extra.Pointer.Event
    | PointerUp Html.Events.Extra.Pointer.Event
    | PointerLeave Html.Events.Extra.Pointer.Event
    | PointerMoved Html.Events.Extra.Pointer.Event
    | PressedLeaveMatch
    | TypedBotCount String
    | PressedCloseMatchEnd


type ScreenCoordinate
    = ScreenCoordinate Never


type alias Mouse =
    { position : Evergreen.V8.Point2d.Point2d Pixels.Pixels ScreenCoordinate
    , primaryDown : Bool
    , secondaryDown : Bool
    }


type alias MatchSetupLocal_ =
    { matchName : String
    , message : String
    , maxPlayers : String
    , botCount : String
    , closedRoundEnd : Bool
    }


type alias Desync =
    { first : Evergreen.V8.NonemptySet.NonemptySet (Evergreen.V8.Id.Id Evergreen.V8.User.UserId)
    , second : Evergreen.V8.NonemptySet.NonemptySet (Evergreen.V8.Id.Id Evergreen.V8.User.UserId)
    , rest : List (Evergreen.V8.NonemptySet.NonemptySet (Evergreen.V8.Id.Id Evergreen.V8.User.UserId))
    }


type alias MatchActiveLocal_ =
    { timelineCache : Result Evergreen.V8.Timeline.Error (Evergreen.V8.Timeline.TimelineCache Evergreen.V8.Match.MatchState)
    , userIds :
        SeqDict.SeqDict
            (Evergreen.V8.Id.Id Evergreen.V8.User.UserId)
            { character : Evergreen.V8.Character.Character
            }
    , wallMesh : Effect.WebGL.Mesh Evergreen.V8.Match.Vertex
    , touchPosition : Maybe (Evergreen.V8.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V8.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , primaryDown : Maybe Effect.Time.Posix
    , previousPrimaryDown : Maybe Effect.Time.Posix
    , desync : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.Timeline.FrameId) Desync
    , footstepMesh : List (Effect.WebGL.Mesh Evergreen.V8.Match.Vertex)
    }


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_
    | MatchError


type alias Model =
    { lobbyId : Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId
    , networkModel :
        Evergreen.V8.NetworkModel.NetworkModel
            { userId : Evergreen.V8.Id.Id Evergreen.V8.User.UserId
            , msg : Evergreen.V8.Match.Msg
            }
            Evergreen.V8.Match.Match
    , matchData : MatchLocalOnly
    }


type alias PlayerPositions =
    { positions : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.User.UserId) (Evergreen.V8.Point2d.Point2d Length.Meters Evergreen.V8.Match.WorldCoordinate)
    , score : Evergreen.V8.Match.Score
    }


type ToBackend
    = MatchRequest (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) (Evergreen.V8.Id.Id Evergreen.V8.NetworkModel.EventId) Evergreen.V8.Match.Msg
    | DesyncCheckRequest (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) (Evergreen.V8.Id.Id Evergreen.V8.Timeline.FrameId) PlayerPositions
    | CurrentCache (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) (Evergreen.V8.Id.Id Evergreen.V8.Timeline.FrameId) Evergreen.V8.Match.MatchState


type ToFrontend
    = MatchSetupBroadcast (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) (Evergreen.V8.Id.Id Evergreen.V8.User.UserId) Evergreen.V8.Match.Msg
    | MatchSetupResponse (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) (Evergreen.V8.Id.Id Evergreen.V8.User.UserId) Evergreen.V8.Match.Msg (Evergreen.V8.Id.Id Evergreen.V8.NetworkModel.EventId)
    | DesyncBroadcast (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) (Evergreen.V8.Id.Id Evergreen.V8.Timeline.FrameId) Desync
    | NeedCurrentCacheBroadcast (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) (Evergreen.V8.Id.Id Evergreen.V8.Timeline.FrameId)
