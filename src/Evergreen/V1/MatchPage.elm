module Evergreen.V1.MatchPage exposing (..)

import Effect.Time
import Effect.WebGL
import Evergreen.V1.Character
import Evergreen.V1.ColorIndex
import Evergreen.V1.Decal
import Evergreen.V1.Id
import Evergreen.V1.Match
import Evergreen.V1.MatchName
import Evergreen.V1.NetworkModel
import Evergreen.V1.Point2d
import Evergreen.V1.SkinTone
import Evergreen.V1.TextMessage
import Evergreen.V1.Timeline
import Evergreen.V1.User
import Html.Events.Extra.Pointer
import Length
import Pixels
import SeqDict


type WorldPixel
    = WorldPixel Never


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor Evergreen.V1.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V1.ColorIndex.ColorIndex
    | PressedDecal (Maybe Evergreen.V1.Decal.Decal)
    | PressedSkinTone Evergreen.V1.SkinTone.SkinTone
    | PressedCharacter Evergreen.V1.Character.Character
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V1.Match.PlayerMode
    | PressedSaveMatchName Evergreen.V1.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V1.TextMessage.TextMessage
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


type MatchId
    = LobbyId Never


type ScreenCoordinate
    = ScreenCoordinate Never


type alias Mouse =
    { position : Evergreen.V1.Point2d.Point2d Pixels.Pixels ScreenCoordinate
    , primaryDown : Bool
    , secondaryDown : Bool
    }


type alias MatchSetupLocal_ =
    { matchName : String
    , message : String
    , maxPlayers : String
    , botCount : String
    }


type alias MatchActiveLocal_ =
    { timelineCache : Result Evergreen.V1.Timeline.Error (Evergreen.V1.Timeline.TimelineCache Evergreen.V1.Match.MatchState)
    , userIds :
        SeqDict.SeqDict
            (Evergreen.V1.Id.Id Evergreen.V1.User.UserId)
            { skinTone : Evergreen.V1.SkinTone.SkinTone
            }
    , wallMesh : Effect.WebGL.Mesh Evergreen.V1.Match.Vertex
    , touchPosition : Maybe (Evergreen.V1.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V1.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , primaryDown : Maybe Effect.Time.Posix
    , previousPrimaryDown : Maybe Effect.Time.Posix
    , desyncedAtFrame : Maybe (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId)
    , footstepMesh : List (Effect.WebGL.Mesh Evergreen.V1.Match.Vertex)
    }


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_
    | MatchError


type alias Model =
    { lobbyId : Evergreen.V1.Id.Id MatchId
    , networkModel :
        Evergreen.V1.NetworkModel.NetworkModel
            { userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
            , msg : Evergreen.V1.Match.Msg
            }
            Evergreen.V1.Match.Match
    , matchData : MatchLocalOnly
    }


type ToBackend
    = MatchRequest (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.NetworkModel.EventId) Evergreen.V1.Match.Msg
    | DesyncCheckRequest (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) (Evergreen.V1.Point2d.Point2d Length.Meters Evergreen.V1.Match.WorldCoordinate))
    | CurrentCache (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId) Evergreen.V1.Match.MatchState


type ToFrontend
    = MatchSetupBroadcast (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) Evergreen.V1.Match.Msg
    | MatchSetupResponse (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) Evergreen.V1.Match.Msg (Evergreen.V1.Id.Id Evergreen.V1.NetworkModel.EventId)
    | DesyncBroadcast (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId)
    | NeedCurrentCacheBroadcast (Evergreen.V1.Id.Id MatchId) (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId)
