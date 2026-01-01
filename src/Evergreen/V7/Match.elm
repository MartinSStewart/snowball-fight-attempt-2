module Evergreen.V7.Match exposing (..)

import Duration
import Effect.WebGL
import Evergreen.V7.Character
import Evergreen.V7.Direction2d
import Evergreen.V7.Id
import Evergreen.V7.MatchName
import Evergreen.V7.Point2d
import Evergreen.V7.Point3d
import Evergreen.V7.TextMessage
import Evergreen.V7.Timeline
import Evergreen.V7.User
import Evergreen.V7.Vector2d
import Evergreen.V7.Vector3d
import Length
import Math.Vector3
import SeqDict
import Speed
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V7.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Action
    = ClickStart (Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate)
    | ClickRelease (Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate)
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
    | SetCharacter Evergreen.V7.Character.Character
    | StartMatch ServerTime
    | MatchInputRequest ServerTime Input
    | SetMatchName Evergreen.V7.MatchName.MatchName
    | SendTextMessage Evergreen.V7.TextMessage.TextMessage
    | MatchFinished Winner
    | SetMaxPlayers Int
    | SetBotCount Int


type alias PlayerData =
    { mode : PlayerMode
    , character : Evergreen.V7.Character.Character
    }


type alias TimelineEvent =
    { userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , input : Input
    }


type alias MatchActive =
    { startTime : ServerTime
    , timeline : Evergreen.V7.Timeline.Timeline TimelineEvent
    }


type alias Match_ =
    { name : Evergreen.V7.MatchName.MatchName
    , owner : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , ownerPlayerData : PlayerData
    , users : SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) PlayerData
    , matchActive : Maybe MatchActive
    , messages :
        List
            { userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
            , message : Evergreen.V7.TextMessage.TextMessage
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
    { position : Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate
    , targetPosition : Maybe (Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate)
    , velocity : Evergreen.V7.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotation : Evergreen.V7.Direction2d.Direction2d WorldCoordinate
    , lastEmote :
        Maybe
            { time : Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId
            , emote : Emote
            }
    , clickStart :
        Maybe
            { position : Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate
            , time : Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId
            }
    , isDead :
        Maybe
            { time : Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId
            , fallDirection : Evergreen.V7.Direction2d.Direction2d WorldCoordinate
            }
    , team : Team
    , lastStep :
        { position : Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate
        , time : Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId
        , stepCount : Int
        }
    }


type alias Snowball =
    { velocity : Evergreen.V7.Vector3d.Vector3d Speed.MetersPerSecond WorldCoordinate
    , position : Evergreen.V7.Point3d.Point3d Length.Meters WorldCoordinate
    , thrownBy : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , thrownAt : Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId
    , apexFrame : Maybe (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId)
    , isOvercharge : Bool
    }


type alias PushableSnowball =
    { position : Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V7.Vector2d.Vector2d Length.Meters WorldCoordinate
    , radius : Length.Length
    }


type alias Particle =
    { position : Evergreen.V7.Point3d.Point3d Length.Meters WorldCoordinate
    , velocity : Evergreen.V7.Vector2d.Vector2d Speed.MetersPerSecond WorldCoordinate
    , size : Length.Length
    , spawnedAt : Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId
    , lifetime : Duration.Duration
    }


type alias Vertex =
    { position : Math.Vector3.Vec3
    , color : Math.Vector3.Vec3
    }


type alias MatchState =
    { players : SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) Player
    , snowballs : List Snowball
    , pushableSnowballs : List PushableSnowball
    , particles : List Particle
    , footsteps :
        List
            { position : Evergreen.V7.Point2d.Point2d Length.Meters WorldCoordinate
            , rotation : Evergreen.V7.Direction2d.Direction2d WorldCoordinate
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
            , time : Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId
            }
    , snowballImpacts : List (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId)
    }
