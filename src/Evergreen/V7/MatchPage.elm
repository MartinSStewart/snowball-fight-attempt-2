module Evergreen.V7.MatchPage exposing (..)

import Effect.Time
import Effect.WebGL
import Evergreen.V7.Character
import Evergreen.V7.Id
import Evergreen.V7.Match
import Evergreen.V7.MatchName
import Evergreen.V7.NetworkModel
import Evergreen.V7.Point2d
import Evergreen.V7.TextMessage
import Evergreen.V7.Timeline
import Evergreen.V7.User
import Html.Events.Extra.Pointer
import Length
import Pixels
import SeqDict


type WorldPixel
    = WorldPixel Never


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedCharacter Evergreen.V7.Character.Character
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V7.Match.PlayerMode
    | PressedSaveMatchName Evergreen.V7.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V7.TextMessage.TextMessage
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


type MatchId
    = LobbyId Never


type ScreenCoordinate
    = ScreenCoordinate Never


type alias Mouse =
    { position : Evergreen.V7.Point2d.Point2d Pixels.Pixels ScreenCoordinate
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


type alias MatchActiveLocal_ =
    { timelineCache : Result Evergreen.V7.Timeline.Error (Evergreen.V7.Timeline.TimelineCache Evergreen.V7.Match.MatchState)
    , userIds :
        SeqDict.SeqDict
            (Evergreen.V7.Id.Id Evergreen.V7.User.UserId)
            { character : Evergreen.V7.Character.Character
            }
    , wallMesh : Effect.WebGL.Mesh Evergreen.V7.Match.Vertex
    , touchPosition : Maybe (Evergreen.V7.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V7.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , primaryDown : Maybe Effect.Time.Posix
    , previousPrimaryDown : Maybe Effect.Time.Posix
    , desyncedAtFrame : Maybe (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId)
    , footstepMesh : List (Effect.WebGL.Mesh Evergreen.V7.Match.Vertex)
    }


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_
    | MatchError


type alias Model =
    { lobbyId : Evergreen.V7.Id.Id MatchId
    , networkModel :
        Evergreen.V7.NetworkModel.NetworkModel
            { userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
            , msg : Evergreen.V7.Match.Msg
            }
            Evergreen.V7.Match.Match
    , matchData : MatchLocalOnly
    }


type ToBackend
    = MatchRequest (Evergreen.V7.Id.Id MatchId) (Evergreen.V7.Id.Id Evergreen.V7.NetworkModel.EventId) Evergreen.V7.Match.Msg
    | DesyncCheckRequest (Evergreen.V7.Id.Id MatchId) (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) (Evergreen.V7.Point2d.Point2d Length.Meters Evergreen.V7.Match.WorldCoordinate))
    | CurrentCache (Evergreen.V7.Id.Id MatchId) (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId) Evergreen.V7.Match.MatchState


type ToFrontend
    = MatchSetupBroadcast (Evergreen.V7.Id.Id MatchId) (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) Evergreen.V7.Match.Msg
    | MatchSetupResponse (Evergreen.V7.Id.Id MatchId) (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) Evergreen.V7.Match.Msg (Evergreen.V7.Id.Id Evergreen.V7.NetworkModel.EventId)
    | DesyncBroadcast (Evergreen.V7.Id.Id MatchId) (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId)
    | NeedCurrentCacheBroadcast (Evergreen.V7.Id.Id MatchId) (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId)
