module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , FrontendLoaded
    , FrontendLoading
    , FrontendModel
    , FrontendModel_(..)
    , FrontendMsg
    , FrontendMsg_(..)
    , JoinLobbyError(..)
    , JoinMatch(..)
    , MainLobbyInitData
    , Page(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Audio
import Browser
import Duration exposing (Duration)
import EditorPage
import Effect.Browser.Navigation
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time as Time
import Effect.WebGL.Texture as Texture exposing (Texture)
import Id exposing (Id)
import Keyboard
import Length exposing (Meters)
import List.Nonempty exposing (Nonempty)
import Match exposing (LobbyPreview, Match, MatchState, ServerTime, WorldCoordinate)
import MatchPage exposing (MatchId, Mouse, PlayerPositions, ScreenCoordinate, WorldPixel)
import NonemptySet exposing (NonemptySet)
import PingData exposing (PingData)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Route exposing (Route)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Size exposing (Size)
import Sounds exposing (Sounds)
import Textures exposing (Textures)
import Timeline exposing (FrameId)
import Url exposing (Url)
import User exposing (BackendUser, UserId)


type alias FrontendModel =
    Audio.Model FrontendMsg_ FrontendModel_


type alias FrontendMsg =
    Audio.Msg FrontendMsg_


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendLoading =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Size
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Maybe Time.Posix
    , debugTimeOffset : Duration
    , initData : Maybe ( Id UserId, MainLobbyInitData )
    , sounds : SeqDict String (Result Audio.LoadError Audio.Source)
    , textures : SeqDict String (Result Texture.Error Texture)
    , route : Route
    }


type alias FrontendLoaded =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Size
    , currentKeys : List Keyboard.Key
    , previousKeys : List Keyboard.Key
    , currentMouse : Mouse
    , previousMouse : Mouse
    , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
    , time : Time.Posix
    , debugTimeOffset : Duration
    , page : Page
    , sounds : Sounds
    , textures : Textures
    , userId : Id UserId
    , pingStartTime : Maybe Time.Posix
    , pingData : Maybe PingData
    , route : Route
    , loadMatchError : Maybe Time.Posix
    , playerNameInput : String
    , currentUser : BackendUser
    , users : SeqDict (Id UserId) BackendUser
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage MatchPage.Model
    | EditorPage EditorPage.Model


type alias MainLobbyPage_ =
    { lobbies : SeqDict (Id MatchId) LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type alias MainLobbyInitData =
    { lobbies : SeqDict (Id MatchId) LobbyPreview, currentUser : BackendUser, users : SeqDict (Id UserId) BackendUser }


type alias BackendModel =
    { userSessions : SeqDict SessionId { clientIds : SeqDict ClientId (), userId : Id UserId }
    , users : SeqDict (Id UserId) BackendUser
    , lobbies : SeqDict (Id MatchId) Match
    , joiningActiveMatch : SeqDict ( Id MatchId, Id FrameId ) (NonemptySet ClientId)
    , counter : Int
    , playerPositions : SeqDict (Id MatchId) (SeqDict (Id FrameId) (SeqDict PlayerPositions (NonemptySet (Id UserId))))
    }


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | KeyMsg Keyboard.Msg
    | WindowResized Size
    | GotDevicePixelRatio (Quantity Float (Rate WorldPixel Pixels))
    | AnimationFrame Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | SoundLoaded String (Result Audio.LoadError Audio.Source)
    | TextureLoaded String (Result Texture.Error Texture)
    | MatchPageMsg MatchPage.Msg
    | GotTime Time.Posix
    | RandomInput Time.Posix
    | EditorPageMsg EditorPage.Msg
    | RejoinMatchTimedOut (Id MatchId)
    | TypedPlayerName String
    | PressedSavePlayerName
    | PressedResetPlayerName


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend MatchPage.ToBackend
    | EditorPageToBackend EditorPage.ToBackend
    | SetNameRequest String


type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId
    | ClientDisconnectedWithTime SessionId ClientId ServerTime
    | UpdateFromFrontendWithTime SessionId ClientId ToBackend ServerTime


type ToFrontend
    = CreateLobbyResponse (Id MatchId) Match
    | RemoveLobbyBroadcast (Id MatchId)
    | UpdateLobbyBroadcast (Id MatchId) LobbyPreview
    | CreateLobbyBroadcast (Id MatchId) LobbyPreview
    | ClientInit (Id UserId) MainLobbyInitData
    | JoinLobbyResponse (Id MatchId) JoinMatch
    | PingResponse ServerTime
    | MatchPageToFrontend MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend EditorPage.ToFrontend
    | SetNameBroadcast (Id UserId) String


type JoinMatch
    = JoinedLobby Match
    | JoinedActiveMatch Match (Id FrameId) MatchState
    | JoinLobbyError JoinLobbyError


type JoinLobbyError
    = MatchNotFound
    | MatchFull
