module Evergreen.V5.MatchPage exposing (..)

import Effect.Time
import Effect.WebGL
import Evergreen.V5.Character
import Evergreen.V5.Id
import Evergreen.V5.Match
import Evergreen.V5.MatchName
import Evergreen.V5.NetworkModel
import Evergreen.V5.Point2d
import Evergreen.V5.TextMessage
import Evergreen.V5.Timeline
import Evergreen.V5.User
import Html.Events.Extra.Pointer
import Length
import Pixels
import SeqDict


type WorldPixel
    = WorldPixel Never


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedCharacter Evergreen.V5.Character.Character
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V5.Match.PlayerMode
    | PressedSaveMatchName Evergreen.V5.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V5.TextMessage.TextMessage
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
    { position : Evergreen.V5.Point2d.Point2d Pixels.Pixels ScreenCoordinate
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
    { timelineCache : Result Evergreen.V5.Timeline.Error (Evergreen.V5.Timeline.TimelineCache Evergreen.V5.Match.MatchState)
    , userIds :
        SeqDict.SeqDict
            (Evergreen.V5.Id.Id Evergreen.V5.User.UserId)
            { character : Evergreen.V5.Character.Character
            }
    , wallMesh : Effect.WebGL.Mesh Evergreen.V5.Match.Vertex
    , touchPosition : Maybe (Evergreen.V5.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V5.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , primaryDown : Maybe Effect.Time.Posix
    , previousPrimaryDown : Maybe Effect.Time.Posix
    , desyncedAtFrame : Maybe (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId)
    , footstepMesh : List (Effect.WebGL.Mesh Evergreen.V5.Match.Vertex)
    }


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_
    | MatchError


type alias Model =
    { lobbyId : Evergreen.V5.Id.Id MatchId
    , networkModel :
        Evergreen.V5.NetworkModel.NetworkModel
            { userId : Evergreen.V5.Id.Id Evergreen.V5.User.UserId
            , msg : Evergreen.V5.Match.Msg
            }
            Evergreen.V5.Match.Match
    , matchData : MatchLocalOnly
    }


type ToBackend
    = MatchRequest (Evergreen.V5.Id.Id MatchId) (Evergreen.V5.Id.Id Evergreen.V5.NetworkModel.EventId) Evergreen.V5.Match.Msg
    | DesyncCheckRequest (Evergreen.V5.Id.Id MatchId) (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.User.UserId) (Evergreen.V5.Point2d.Point2d Length.Meters Evergreen.V5.Match.WorldCoordinate))
    | CurrentCache (Evergreen.V5.Id.Id MatchId) (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId) Evergreen.V5.Match.MatchState


type ToFrontend
    = MatchSetupBroadcast (Evergreen.V5.Id.Id MatchId) (Evergreen.V5.Id.Id Evergreen.V5.User.UserId) Evergreen.V5.Match.Msg
    | MatchSetupResponse (Evergreen.V5.Id.Id MatchId) (Evergreen.V5.Id.Id Evergreen.V5.User.UserId) Evergreen.V5.Match.Msg (Evergreen.V5.Id.Id Evergreen.V5.NetworkModel.EventId)
    | DesyncBroadcast (Evergreen.V5.Id.Id MatchId) (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId)
    | NeedCurrentCacheBroadcast (Evergreen.V5.Id.Id MatchId) (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId)
