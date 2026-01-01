module Evergreen.V5.Match exposing (..)

import Duration
import Effect.WebGL
import Evergreen.V5.Character
import Evergreen.V5.Direction2d
import Evergreen.V5.Id
import Evergreen.V5.MatchName
import Evergreen.V5.Point2d
import Evergreen.V5.Point3d
import Evergreen.V5.TextMessage
import Evergreen.V5.Timeline
import Evergreen.V5.User
import Evergreen.V5.Vector2d
import Evergreen.V5.Vector3d
import Length
import Math.Vector3
import SeqDict
import Speed
import Time


type PlayerMode
    = PlayerMode
    | SpectatorMode


type alias LobbyPreview =
    { name : Evergreen.V5.MatchName.MatchName
    , userCount : Int
    , maxUserCount : Int
    }


type ServerTime
    = ServerTime Time.Posix


type WorldCoordinate
    = WorldCoordinate Never


type Action
    = ClickStart (Evergreen.V5.Point2d.Point2d Length.Meters WorldCoordinate)
    | ClickRelease (Evergreen.V5.Point2d.Point2d Length.Meters WorldCoordinate)
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
    | SetCharacter Evergreen.V5.Character.Character
    | StartMatch ServerTime
    | MatchInputRequest ServerTime Input
    | SetMatchName Evergreen.V5.MatchName.MatchName
    | SendTextMessage Evergreen.V5.TextMessage.TextMessage
    | MatchFinished Winner
    | SetMaxPlayers Int
    | SetBotCount Int


type alias PlayerData =
    { mode : PlayerMode
    , character : Evergreen.V5.Character.Character
    }


type alias TimelineEvent =
    { userId : Evergreen.V5.Id.Id Evergreen.V5.User.UserId
    , input : Input
    }


type alias MatchActive =
    { startTime : ServerTime
    , timeline : Evergreen.V5.Timeline.Timeline TimelineEvent
    }


type alias Match_ =
    { name : Evergreen.V5.MatchName.MatchName
    , owner : Evergreen.V5.Id.Id Evergreen.V5.User.UserId
    , ownerPlayerData : PlayerData
    , users : SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.User.UserId) PlayerData
    , matchActive : Maybe MatchActive
    , messages :
        List
            { userId : Evergreen.V5.Id.Id Evergreen.V5.User.UserId
            , message : Evergreen.V5.TextMessage.TextMessage
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
    { position : Evergreen.V5.Point2d.Point2d Length.Meters WorldCoordinate
    , targetPosition : Maybe (Evergreen.V5.Point2d.Point2d Length.Meters WorldCoordinate)
    , velocity : Evergreen.V5.Vector2d.Vector2d Length.Meters WorldCoordinate
    , rotation : Evergreen.V5.Direction2d.Direction2d WorldCoordinate
    , lastEmote :
        Maybe
            { time : Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId
            , emote : Emote
            }
    , clickStart :
        Maybe
            { position : Evergreen.V5.Point2d.Point2d Length.Meters WorldCoordinate
            , time : Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId
            }
    , isDead :
        Maybe
            { time : Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId
            , fallDirection : Evergreen.V5.Direction2d.Direction2d WorldCoordinate
            }
    , team : Team
    , lastStep :
        { position : Evergreen.V5.Point2d.Point2d Length.Meters WorldCoordinate
        , time : Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId
        , stepCount : Int
        }
    }


type alias Snowball =
    { velocity : Evergreen.V5.Vector3d.Vector3d Speed.MetersPerSecond WorldCoordinate
    , position : Evergreen.V5.Point3d.Point3d Length.Meters WorldCoordinate
    , thrownBy : Evergreen.V5.Id.Id Evergreen.V5.User.UserId
    , thrownAt : Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId
    , apexFrame : Maybe (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId)
    , isOvercharge : Bool
    }


type alias PushableSnowball =
    { position : Evergreen.V5.Point2d.Point2d Length.Meters WorldCoordinate
    , velocity : Evergreen.V5.Vector2d.Vector2d Length.Meters WorldCoordinate
    , radius : Length.Length
    }


type alias Particle =
    { position : Evergreen.V5.Point3d.Point3d Length.Meters WorldCoordinate
    , velocity : Evergreen.V5.Vector2d.Vector2d Speed.MetersPerSecond WorldCoordinate
    , size : Length.Length
    , spawnedAt : Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId
    , lifetime : Duration.Duration
    }


type alias Vertex =
    { position : Math.Vector3.Vec3
    , color : Math.Vector3.Vec3
    }


type alias MatchState =
    { players : SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.User.UserId) Player
    , snowballs : List Snowball
    , pushableSnowballs : List PushableSnowball
    , particles : List Particle
    , footsteps :
        List
            { position : Evergreen.V5.Point2d.Point2d Length.Meters WorldCoordinate
            , rotation : Evergreen.V5.Direction2d.Direction2d WorldCoordinate
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
            , time : Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId
            }
    , snowballImpacts : List (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId)
    }
