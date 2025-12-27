module Evergreen.V1.Match exposing (..)

import Duration
import Effect.WebGL
import Evergreen.V1.Character
import Evergreen.V1.ColorIndex
import Evergreen.V1.Decal
import Evergreen.V1.Direction2d
import Evergreen.V1.Id
import Evergreen.V1.MatchName
import Evergreen.V1.Point2d
import Evergreen.V1.Point3d
import Evergreen.V1.SkinTone
import Evergreen.V1.TextMessage
import Evergreen.V1.Timeline
import Evergreen.V1.User
import Evergreen.V1.Vector2d
import Evergreen.V1.Vector3d
import Length
import Math.Vector3
import SeqDict
import Speed
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V1.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Action
    = ClickStart (Evergreen.V1.Point2d.Point2d Length.Meters WorldCoordinate)
    | ClickRelease (Evergreen.V1.Point2d.Point2d Length.Meters WorldCoordinate)
    | NoAction


type Emote
    = SurpriseEmote
    | ImpEmote


type alias Input =
    { action : Action
    , emote : Maybe Emote
    }


type Winner
    = BothLost
    | RedWon
    | BlueWon


type Msg
    = JoinMatchSetup
    | LeaveMatchSetup
    | SetPrimaryColor Evergreen.V1.ColorIndex.ColorIndex
    | SetSecondaryColor Evergreen.V1.ColorIndex.ColorIndex
    | SetDecal (Maybe Evergreen.V1.Decal.Decal)
    | SetSkinTone Evergreen.V1.SkinTone.SkinTone
    | SetPlayerMode PlayerMode
    | SetCharacter Evergreen.V1.Character.Character
    | StartMatch ServerTime
    | MatchInputRequest ServerTime Input
    | SetMatchName Evergreen.V1.MatchName.MatchName
    | SendTextMessage Evergreen.V1.TextMessage.TextMessage
    | MatchFinished Winner
    | SetMaxPlayers Int
    | SetBotCount Int


type alias PlayerData =
    { primaryColor : Evergreen.V1.ColorIndex.ColorIndex
    , secondaryColor : Evergreen.V1.ColorIndex.ColorIndex
    , decal : Maybe Evergreen.V1.Decal.Decal
    , mode : PlayerMode
    , skinTone : Evergreen.V1.SkinTone.SkinTone
    , character : Evergreen.V1.Character.Character
    }


type alias TimelineEvent =
    { userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , input : Input
    }


type alias MatchActive =
    { startTime : ServerTime
    , timeline : Evergreen.V1.Timeline.Timeline TimelineEvent
    }


type alias Match_ =
    { name : Evergreen.V1.MatchName.MatchName
    , owner : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , ownerPlayerData : PlayerData
    , users : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) PlayerData
    , matchActive : Maybe MatchActive
    , messages :
        List
            { userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
            , message : Evergreen.V1.TextMessage.TextMessage
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
    { position : Evergreen.V1.Point2d.Point2d Length.Meters WorldCoordinate
    , targetPosition : Maybe (Evergreen.V1.Point2d.Point2d Length.Meters WorldCoordinate)
    , velocity : Evergreen.V1.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotation : Evergreen.V1.Direction2d.Direction2d WorldCoordinate
    , lastCollision : Maybe (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId)
    , lastEmote :
        Maybe
            { time : Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId
            , emote : Emote
            }
    , clickStart :
        Maybe
            { position : Evergreen.V1.Point2d.Point2d Length.Meters WorldCoordinate
            , time : Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId
            }
    , isDead :
        Maybe
            { time : Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId
            , fallDirection : Evergreen.V1.Direction2d.Direction2d WorldCoordinate
            }
    , team : Team
    , lastStep :
        { position : Evergreen.V1.Point2d.Point2d Length.Meters WorldCoordinate
        , time : Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId
        , stepCount : Int
        }
    }


type alias Snowball =
    { velocity : Evergreen.V1.Vector3d.Vector3d Speed.MetersPerSecond WorldCoordinate
    , position : Evergreen.V1.Point3d.Point3d Length.Meters WorldCoordinate
    , thrownBy : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , thrownAt : Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId
    }


type alias Particle =
    { position : Evergreen.V1.Point3d.Point3d Length.Meters WorldCoordinate
    , velocity : Evergreen.V1.Vector2d.Vector2d Speed.MetersPerSecond WorldCoordinate
    , size : Length.Length
    , spawnedAt : Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId
    , lifetime : Duration.Duration
    }


type alias Vertex =
    { position : Math.Vector3.Vec3
    , color : Math.Vector3.Vec3
    }


type alias MatchState =
    { players : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) Player
    , snowballs : List Snowball
    , particles : List Particle
    , footsteps :
        List
            { position : Evergreen.V1.Point2d.Point2d Length.Meters WorldCoordinate
            , rotation : Evergreen.V1.Direction2d.Direction2d WorldCoordinate
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
            , time : Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId
            }
    }
