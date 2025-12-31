module Evergreen.V3.Match exposing (..)

import Duration
import Effect.WebGL
import Evergreen.V3.Character
import Evergreen.V3.Direction2d
import Evergreen.V3.Id
import Evergreen.V3.MatchName
import Evergreen.V3.Point2d
import Evergreen.V3.Point3d
import Evergreen.V3.TextMessage
import Evergreen.V3.Timeline
import Evergreen.V3.User
import Evergreen.V3.Vector2d
import Evergreen.V3.Vector3d
import Length
import Math.Vector3
import SeqDict
import Speed
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V3.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Action
    = ClickStart (Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate)
    | ClickRelease (Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate)
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
    | SetPlayerMode PlayerMode
    | SetCharacter Evergreen.V3.Character.Character
    | StartMatch ServerTime
    | MatchInputRequest ServerTime Input
    | SetMatchName Evergreen.V3.MatchName.MatchName
    | SendTextMessage Evergreen.V3.TextMessage.TextMessage
    | MatchFinished Winner
    | SetMaxPlayers Int
    | SetBotCount Int


type alias PlayerData =
    { mode : PlayerMode
    , character : Evergreen.V3.Character.Character
    }


type alias TimelineEvent =
    { userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , input : Input
    }


type alias MatchActive =
    { startTime : ServerTime
    , timeline : Evergreen.V3.Timeline.Timeline TimelineEvent
    }


type alias Match_ =
    { name : Evergreen.V3.MatchName.MatchName
    , owner : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , ownerPlayerData : PlayerData
    , users : SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) PlayerData
    , matchActive : Maybe MatchActive
    , messages :
        List
            { userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
            , message : Evergreen.V3.TextMessage.TextMessage
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
    { position : Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate
    , targetPosition : Maybe (Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate)
    , velocity : Evergreen.V3.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotation : Evergreen.V3.Direction2d.Direction2d WorldCoordinate
    , lastEmote :
        Maybe
            { time : Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId
            , emote : Emote
            }
    , clickStart :
        Maybe
            { position : Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate
            , time : Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId
            }
    , isDead :
        Maybe
            { time : Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId
            , fallDirection : Evergreen.V3.Direction2d.Direction2d WorldCoordinate
            }
    , team : Team
    , lastStep :
        { position : Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate
        , time : Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId
        , stepCount : Int
        }
    }


type alias Snowball =
    { velocity : Evergreen.V3.Vector3d.Vector3d Speed.MetersPerSecond WorldCoordinate
    , position : Evergreen.V3.Point3d.Point3d Length.Meters WorldCoordinate
    , thrownBy : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , thrownAt : Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId
    , apexFrame : Maybe (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId)
    , isOvercharge : Bool
    }


type alias PushableSnowball =
    { position : Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V3.Vector2d.Vector2d Length.Meters WorldCoordinate
    , radius : Length.Length
    }


type alias Particle =
    { position : Evergreen.V3.Point3d.Point3d Length.Meters WorldCoordinate
    , velocity : Evergreen.V3.Vector2d.Vector2d Speed.MetersPerSecond WorldCoordinate
    , size : Length.Length
    , spawnedAt : Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId
    , lifetime : Duration.Duration
    }


type alias Vertex =
    { position : Math.Vector3.Vec3
    , color : Math.Vector3.Vec3
    }


type alias MatchState =
    { players : SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) Player
    , snowballs : List Snowball
    , pushableSnowballs : List PushableSnowball
    , particles : List Particle
    , footsteps :
        List
            { position : Evergreen.V3.Point2d.Point2d Length.Meters WorldCoordinate
            , rotation : Evergreen.V3.Direction2d.Direction2d WorldCoordinate
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
            , time : Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId
            }
    , snowballImpacts : List (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId)
    }
