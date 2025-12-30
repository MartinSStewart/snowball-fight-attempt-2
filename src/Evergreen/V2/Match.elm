module Evergreen.V2.Match exposing (..)

import Duration
import Effect.WebGL
import Evergreen.V2.Character
import Evergreen.V2.ColorIndex
import Evergreen.V2.Decal
import Evergreen.V2.Direction2d
import Evergreen.V2.Id
import Evergreen.V2.MatchName
import Evergreen.V2.Point2d
import Evergreen.V2.Point3d
import Evergreen.V2.SkinTone
import Evergreen.V2.TextMessage
import Evergreen.V2.Timeline
import Evergreen.V2.User
import Evergreen.V2.Vector2d
import Evergreen.V2.Vector3d
import Length
import Math.Vector3
import SeqDict
import Speed
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V2.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Action
    = ClickStart (Evergreen.V2.Point2d.Point2d Length.Meters WorldCoordinate)
    | ClickRelease (Evergreen.V2.Point2d.Point2d Length.Meters WorldCoordinate)
    | NoAction


type Emote
    = SurpriseEmote
    | ImpEmote


type alias Input =
    { action : Action
    , emote : Maybe Emote
    }


type Winner
    = BothWon
    | RedWon
    | BlueWon


type Msg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V2.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V2.ColorIndex.ColorIndex
    | SetDecal (Maybe Evergreen.V2.Decal.Decal)
    | SetSkinTone Evergreen.V2.SkinTone.SkinTone
    | SetPlayerMode PlayerMode
    | SetCharacter Evergreen.V2.Character.Character
    | StartMatch ServerTime
    | MatchInputRequest ServerTime Input
    | SetMatchName Evergreen.V2.MatchName.MatchName
    | SendTextMessage Evergreen.V2.TextMessage.TextMessage
    | MatchFinished Winner
    | SetMaxPlayers Int
    | SetBotCount Int


type alias PlayerData =
    { primaryColor : Evergreen.V2.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V2.ColorIndex.ColorIndex
    , decal : Maybe Evergreen.V2.Decal.Decal
    , mode : PlayerMode
    , skinTone : Evergreen.V2.SkinTone.SkinTone
    , character : Evergreen.V2.Character.Character
    }


type alias TimelineEvent =
    { userId : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
    , input : Input
    }


type alias MatchActive =
    { startTime : ServerTime
    , timeline : Evergreen.V2.Timeline.Timeline TimelineEvent
    }


type alias Match_ =
    { name : Evergreen.V2.MatchName.MatchName
    , owner : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
    , ownerPlayerData : PlayerData
    , users : SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) PlayerData
    , matchActive : Maybe MatchActive
    , messages :
        List
            { userId : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
            , message : Evergreen.V2.TextMessage.TextMessage
            }
    , previousMatch : Maybe Winner
    , maxPlayers : Int
    , botCount : Int
    }


type Match
    = Match Match_


type Team
    = RedTeam
    | BlueTeam


type alias Player =
    { position : Evergreen.V2.Point2d.Point2d Length.Meters WorldCoordinate
    , targetPosition : Maybe (Evergreen.V2.Point2d.Point2d Length.Meters WorldCoordinate)
    , velocity : Evergreen.V2.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotation : Evergreen.V2.Direction2d.Direction2d WorldCoordinate
    , lastEmote :
        Maybe
            { time : Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId
            , emote : Emote
            }
    , clickStart :
        Maybe
            { position : Evergreen.V2.Point2d.Point2d Length.Meters WorldCoordinate
            , time : Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId
            }
    , isDead :
        Maybe
            { time : Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId
            , fallDirection : Evergreen.V2.Direction2d.Direction2d WorldCoordinate
            }
    , team : Team
    , lastStep :
        { position : Evergreen.V2.Point2d.Point2d Length.Meters WorldCoordinate
        , time : Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId
        , stepCount : Int
        }
    }


type alias Snowball =
    { velocity : Evergreen.V2.Vector3d.Vector3d Speed.MetersPerSecond WorldCoordinate
    , position : Evergreen.V2.Point3d.Point3d Length.Meters WorldCoordinate
    , thrownBy : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
    , thrownAt : Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId
    , apexFrame : Maybe (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId)
    , isOvercharge : Bool
    }


type alias PushableSnowball =
    { position : Evergreen.V2.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V2.Vector2d.Vector2d Length.Meters WorldCoordinate
    , radius : Length.Length
    }


type alias Particle =
    { position : Evergreen.V2.Point3d.Point3d Length.Meters WorldCoordinate
    , velocity : Evergreen.V2.Vector2d.Vector2d Speed.MetersPerSecond WorldCoordinate
    , size : Length.Length
    , spawnedAt : Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId
    , lifetime : Duration.Duration
    }


type alias Vertex =
    { position : Math.Vector3.Vec3
    , color : Math.Vector3.Vec3
    }


type alias MatchState =
    { players : SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) Player
    , snowballs : List Snowball
    , pushableSnowballs : List PushableSnowball
    , particles : List Particle
    , footsteps :
        List
            { position : Evergreen.V2.Point2d.Point2d Length.Meters WorldCoordinate
            , rotation : Evergreen.V2.Direction2d.Direction2d WorldCoordinate
            , stepCount : Int
            , mesh : Effect.WebGL.Mesh Vertex
            }
    , mergedFootsteps : List (Effect.WebGL.Mesh Vertex)
    , score :
        { redTeam : Int
        , blueTeam : Int
        }
    , roundEndTime :
        Maybe
            { winner : Winner
            , time : Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId
            }
    , snowballImpacts : List (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId)
    }
