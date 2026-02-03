module Evergreen.V8.Types exposing (..)

import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL.Texture
import Evergreen.V8.Audio
import Evergreen.V8.EditorPage
import Evergreen.V8.Id
import Evergreen.V8.Keyboard
import Evergreen.V8.Match
import Evergreen.V8.MatchPage
import Evergreen.V8.NonemptySet
import Evergreen.V8.PingData
import Evergreen.V8.Route
import Evergreen.V8.Size
import Evergreen.V8.Sounds
import Evergreen.V8.Textures
import Evergreen.V8.Timeline
import Evergreen.V8.User
import Pixels
import Quantity
import SeqDict
import Url


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Evergreen.V8.Keyboard.Msg
    | WindowResized Evergreen.V8.Size.Size
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V8.MatchPage.WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | SoundLoaded String (Result Evergreen.V8.Audio.LoadError Evergreen.V8.Audio.Source)
    | TextureLoaded String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    | MatchPageMsg Evergreen.V8.MatchPage.Msg
    | GotTime Effect.Time.Posix
    | RandomInput Effect.Time.Posix
    | EditorPageMsg Evergreen.V8.EditorPage.Msg
    | RejoinMatchTimedOut (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId)
    | TypedPlayerName String
    | PressedSavePlayerName
    | PressedResetPlayerName


type alias MainLobbyInitData =
    { lobbies : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) Evergreen.V8.Match.LobbyPreview
    , currentUser : Evergreen.V8.User.BackendUser
    , users : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.User.UserId) Evergreen.V8.User.BackendUser
    }


type alias FrontendLoading =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V8.Size.Size
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V8.MatchPage.WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V8.Id.Id Evergreen.V8.User.UserId, MainLobbyInitData )
    , sounds : SeqDict.SeqDict String (Result Evergreen.V8.Audio.LoadError Evergreen.V8.Audio.Source)
    , textures : SeqDict.SeqDict String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    , route : Evergreen.V8.Route.Route
    }


type JoinLobbyError
    = MatchNotFound
    | MatchFull


type alias MainLobbyPage_ =
    { lobbies : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) Evergreen.V8.Match.LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage Evergreen.V8.MatchPage.Model
    | EditorPage Evergreen.V8.EditorPage.Model


type alias FrontendLoaded =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V8.Size.Size
    , currentKeys : List Evergreen.V8.Keyboard.Key
    , previousKeys : List Evergreen.V8.Keyboard.Key
    , currentMouse : Evergreen.V8.MatchPage.Mouse
    , previousMouse : Evergreen.V8.MatchPage.Mouse
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V8.MatchPage.WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V8.Sounds.Sounds
    , textures : Evergreen.V8.Textures.Textures
    , userId : Evergreen.V8.Id.Id Evergreen.V8.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe Evergreen.V8.PingData.PingData
    , route : Evergreen.V8.Route.Route
    , loadMatchError : Maybe Effect.Time.Posix
    , playerNameInput : String
    , currentUser : Evergreen.V8.User.BackendUser
    , users : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.User.UserId) Evergreen.V8.User.BackendUser
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V8.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendModel =
    { userSessions :
        SeqDict.SeqDict
            Effect.Lamdera.SessionId
            { clientIds : SeqDict.SeqDict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V8.Id.Id Evergreen.V8.User.UserId
            }
    , users : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.User.UserId) Evergreen.V8.User.BackendUser
    , lobbies : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) Evergreen.V8.Match.Match
    , joiningActiveMatch : SeqDict.SeqDict ( Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId, Evergreen.V8.Id.Id Evergreen.V8.Timeline.FrameId ) (Evergreen.V8.NonemptySet.NonemptySet Effect.Lamdera.ClientId)
    , counter : Int
    , playerPositions : SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) (SeqDict.SeqDict (Evergreen.V8.Id.Id Evergreen.V8.Timeline.FrameId) (SeqDict.SeqDict Evergreen.V8.MatchPage.PlayerPositions (Evergreen.V8.NonemptySet.NonemptySet (Evergreen.V8.Id.Id Evergreen.V8.User.UserId))))
    }


type alias FrontendMsg =
    Evergreen.V8.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend Evergreen.V8.MatchPage.ToBackend
    | EditorPageToBackend Evergreen.V8.EditorPage.ToBackend
    | SetNameRequest String


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V8.Match.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V8.Match.ServerTime


type JoinMatch
    = JoinedLobby Evergreen.V8.Match.Match
    | JoinedActiveMatch Evergreen.V8.Match.Match (Evergreen.V8.Id.Id Evergreen.V8.Timeline.FrameId) Evergreen.V8.Match.MatchState
    | JoinLobbyError JoinLobbyError


type ToFrontend
    = CreateLobbyResponse (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) Evergreen.V8.Match.Match
    | RemoveLobbyBroadcast (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId)
    | UpdateLobbyBroadcast (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) Evergreen.V8.Match.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) Evergreen.V8.Match.LobbyPreview
    | ClientInit (Evergreen.V8.Id.Id Evergreen.V8.User.UserId) MainLobbyInitData
    | JoinLobbyResponse (Evergreen.V8.Id.Id Evergreen.V8.Id.MatchId) JoinMatch
    | PingResponse Evergreen.V8.Match.ServerTime
    | MatchPageToFrontend Evergreen.V8.MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend Evergreen.V8.EditorPage.ToFrontend
    | SetNameBroadcast (Evergreen.V8.Id.Id Evergreen.V8.User.UserId) String
