module Evergreen.V7.Types exposing (..)

import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL.Texture
import Evergreen.V7.Audio
import Evergreen.V7.EditorPage
import Evergreen.V7.Id
import Evergreen.V7.Keyboard
import Evergreen.V7.Match
import Evergreen.V7.MatchPage
import Evergreen.V7.NonemptySet
import Evergreen.V7.PingData
import Evergreen.V7.Point2d
import Evergreen.V7.Route
import Evergreen.V7.Size
import Evergreen.V7.Sounds
import Evergreen.V7.Textures
import Evergreen.V7.Timeline
import Evergreen.V7.User
import Length
import Pixels
import Quantity
import SeqDict
import Url


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Evergreen.V7.Keyboard.Msg
    | WindowResized Evergreen.V7.Size.Size
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V7.MatchPage.WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | SoundLoaded String (Result Evergreen.V7.Audio.LoadError Evergreen.V7.Audio.Source)
    | TextureLoaded String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    | MatchPageMsg Evergreen.V7.MatchPage.Msg
    | GotTime Effect.Time.Posix
    | RandomInput Effect.Time.Posix
    | EditorPageMsg Evergreen.V7.EditorPage.Msg
    | RejoinMatchTimedOut (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId)


type alias MainLobbyInitData =
    { lobbies : SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId) Evergreen.V7.Match.LobbyPreview
    }


type alias FrontendLoading =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V7.Size.Size
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V7.MatchPage.WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V7.Id.Id Evergreen.V7.User.UserId, MainLobbyInitData )
    , sounds : SeqDict.SeqDict String (Result Evergreen.V7.Audio.LoadError Evergreen.V7.Audio.Source)
    , textures : SeqDict.SeqDict String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    , route : Evergreen.V7.Route.Route
    }


type JoinLobbyError
    = MatchNotFound
    | MatchFull


type alias MainLobbyPage_ =
    { lobbies : SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId) Evergreen.V7.Match.LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage Evergreen.V7.MatchPage.Model
    | EditorPage Evergreen.V7.EditorPage.Model


type alias FrontendLoaded =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V7.Size.Size
    , currentKeys : List Evergreen.V7.Keyboard.Key
    , previousKeys : List Evergreen.V7.Keyboard.Key
    , currentMouse : Evergreen.V7.MatchPage.Mouse
    , previousMouse : Evergreen.V7.MatchPage.Mouse
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V7.MatchPage.WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V7.Sounds.Sounds
    , textures : Evergreen.V7.Textures.Textures
    , userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe Evergreen.V7.PingData.PingData
    , route : Evergreen.V7.Route.Route
    , loadMatchError : Maybe Effect.Time.Posix
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V7.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        SeqDict.SeqDict
            Effect.Lamdera.SessionId
            { clientIds : SeqDict.SeqDict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V7.Id.Id Evergreen.V7.User.UserId
            }
    , users : SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) BackendUserData
    , lobbies : SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId) Evergreen.V7.Match.Match
    , joiningActiveMatch : SeqDict.SeqDict ( Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId, Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId ) (Evergreen.V7.NonemptySet.NonemptySet Effect.Lamdera.ClientId)
    , dummyChange : Float
    , counter : Int
    , playerPositions : SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId) (SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) (Evergreen.V7.Point2d.Point2d Length.Meters Evergreen.V7.Match.WorldCoordinate)))
    }


type alias FrontendMsg =
    Evergreen.V7.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend Evergreen.V7.MatchPage.ToBackend
    | EditorPageToBackend Evergreen.V7.EditorPage.ToBackend


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V7.Match.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V7.Match.ServerTime


type JoinMatch
    = JoinedLobby Evergreen.V7.Match.Match
    | JoinedActiveMatch Evergreen.V7.Match.Match (Evergreen.V7.Id.Id Evergreen.V7.Timeline.FrameId) Evergreen.V7.Match.MatchState
    | JoinLobbyError JoinLobbyError


type ToFrontend
    = CreateLobbyResponse (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId) Evergreen.V7.Match.Match
    | RemoveLobbyBroadcast (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId)
    | UpdateLobbyBroadcast (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId) Evergreen.V7.Match.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId) Evergreen.V7.Match.LobbyPreview
    | ClientInit (Evergreen.V7.Id.Id Evergreen.V7.User.UserId) MainLobbyInitData
    | JoinLobbyResponse (Evergreen.V7.Id.Id Evergreen.V7.MatchPage.MatchId) JoinMatch
    | PingResponse Evergreen.V7.Match.ServerTime
    | MatchPageToFrontend Evergreen.V7.MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend Evergreen.V7.EditorPage.ToFrontend
