module Evergreen.V3.MatchPage exposing (..)

import Effect.Time
import Effect.WebGL
import Evergreen.V3.Character
import Evergreen.V3.Id
import Evergreen.V3.Match
import Evergreen.V3.MatchName
import Evergreen.V3.NetworkModel
import Evergreen.V3.Point2d
import Evergreen.V3.TextMessage
import Evergreen.V3.Timeline
import Evergreen.V3.User
import Html.Events.Extra.Pointer
import Length
import Pixels
import SeqDict


type WorldPixel
    = WorldPixel Never


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedCharacter Evergreen.V3.Character.Character
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V3.Match.PlayerMode
    | PressedSaveMatchName Evergreen.V3.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V3.TextMessage.TextMessage
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
    { position : Evergreen.V3.Point2d.Point2d Pixels.Pixels ScreenCoordinate
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
    { timelineCache : Result Evergreen.V3.Timeline.Error (Evergreen.V3.Timeline.TimelineCache Evergreen.V3.Match.MatchState)
    , userIds :
        SeqDict.SeqDict
            (Evergreen.V3.Id.Id Evergreen.V3.User.UserId)
            { character : Evergreen.V3.Character.Character
            }
    , wallMesh : Effect.WebGL.Mesh Evergreen.V3.Match.Vertex
    , touchPosition : Maybe (Evergreen.V3.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V3.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , primaryDown : Maybe Effect.Time.Posix
    , previousPrimaryDown : Maybe Effect.Time.Posix
    , desyncedAtFrame : Maybe (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId)
    , footstepMesh : List (Effect.WebGL.Mesh Evergreen.V3.Match.Vertex)
    }


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_
    | MatchError


type alias Model =
    { lobbyId : Evergreen.V3.Id.Id MatchId
    , networkModel :
        Evergreen.V3.NetworkModel.NetworkModel
            { userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
            , msg : Evergreen.V3.Match.Msg
            }
            Evergreen.V3.Match.Match
    , matchData : MatchLocalOnly
    }


type ToBackend
    = MatchRequest (Evergreen.V3.Id.Id MatchId) (Evergreen.V3.Id.Id Evergreen.V3.NetworkModel.EventId) Evergreen.V3.Match.Msg
    | DesyncCheckRequest (Evergreen.V3.Id.Id MatchId) (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) (Evergreen.V3.Point2d.Point2d Length.Meters Evergreen.V3.Match.WorldCoordinate))
    | CurrentCache (Evergreen.V3.Id.Id MatchId) (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId) Evergreen.V3.Match.MatchState


type ToFrontend
    = MatchSetupBroadcast (Evergreen.V3.Id.Id MatchId) (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) Evergreen.V3.Match.Msg
    | MatchSetupResponse (Evergreen.V3.Id.Id MatchId) (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) Evergreen.V3.Match.Msg (Evergreen.V3.Id.Id Evergreen.V3.NetworkModel.EventId)
    | DesyncBroadcast (Evergreen.V3.Id.Id MatchId) (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId)
    | NeedCurrentCacheBroadcast (Evergreen.V3.Id.Id MatchId) (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId)
