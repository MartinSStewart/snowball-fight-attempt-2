module Evergreen.V2.MatchPage exposing (..)

import Effect.Time
import Effect.WebGL
import Evergreen.V2.Character
import Evergreen.V2.ColorIndex
import Evergreen.V2.Decal
import Evergreen.V2.Id
import Evergreen.V2.Match
import Evergreen.V2.MatchName
import Evergreen.V2.NetworkModel
import Evergreen.V2.Point2d
import Evergreen.V2.SkinTone
import Evergreen.V2.TextMessage
import Evergreen.V2.Timeline
import Evergreen.V2.User
import Html.Events.Extra.Pointer
import Length
import Pixels
import SeqDict


type WorldPixel
    = WorldPixel Never


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor Evergreen.V2.ColorIndex.ColorIndex
    | PressedSecondaryColor Evergreen.V2.ColorIndex.ColorIndex
    | PressedDecal (Maybe Evergreen.V2.Decal.Decal)
    | PressedSkinTone Evergreen.V2.SkinTone.SkinTone
    | PressedCharacter Evergreen.V2.Character.Character
    | TypedMatchName String
    | PressedPlayerMode Evergreen.V2.Match.PlayerMode
    | PressedSaveMatchName Evergreen.V2.MatchName.MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage Evergreen.V2.TextMessage.TextMessage
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
    { position : Evergreen.V2.Point2d.Point2d Pixels.Pixels ScreenCoordinate
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
    { timelineCache : Result Evergreen.V2.Timeline.Error (Evergreen.V2.Timeline.TimelineCache Evergreen.V2.Match.MatchState)
    , userIds :
        SeqDict.SeqDict
            (Evergreen.V2.Id.Id Evergreen.V2.User.UserId)
            { skinTone : Evergreen.V2.SkinTone.SkinTone
            , character : Evergreen.V2.Character.Character
            }
    , wallMesh : Effect.WebGL.Mesh Evergreen.V2.Match.Vertex
    , touchPosition : Maybe (Evergreen.V2.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Evergreen.V2.Point2d.Point2d Pixels.Pixels ScreenCoordinate)
    , primaryDown : Maybe Effect.Time.Posix
    , previousPrimaryDown : Maybe Effect.Time.Posix
    , desyncedAtFrame : Maybe (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId)
    , footstepMesh : List (Effect.WebGL.Mesh Evergreen.V2.Match.Vertex)
    }


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_
    | MatchError


type alias Model =
    { lobbyId : Evergreen.V2.Id.Id MatchId
    , networkModel :
        Evergreen.V2.NetworkModel.NetworkModel
            { userId : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
            , msg : Evergreen.V2.Match.Msg
            }
            Evergreen.V2.Match.Match
    , matchData : MatchLocalOnly
    }


type ToBackend
    = MatchRequest (Evergreen.V2.Id.Id MatchId) (Evergreen.V2.Id.Id Evergreen.V2.NetworkModel.EventId) Evergreen.V2.Match.Msg
    | DesyncCheckRequest (Evergreen.V2.Id.Id MatchId) (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) (Evergreen.V2.Point2d.Point2d Length.Meters Evergreen.V2.Match.WorldCoordinate))
    | CurrentCache (Evergreen.V2.Id.Id MatchId) (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId) Evergreen.V2.Match.MatchState


type ToFrontend
    = MatchSetupBroadcast (Evergreen.V2.Id.Id MatchId) (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) Evergreen.V2.Match.Msg
    | MatchSetupResponse (Evergreen.V2.Id.Id MatchId) (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) Evergreen.V2.Match.Msg (Evergreen.V2.Id.Id Evergreen.V2.NetworkModel.EventId)
    | DesyncBroadcast (Evergreen.V2.Id.Id MatchId) (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId)
    | NeedCurrentCacheBroadcast (Evergreen.V2.Id.Id MatchId) (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId)
