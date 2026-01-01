module Evergreen.V5.Types exposing (..)

import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL.Texture
import Evergreen.V5.Audio
import Evergreen.V5.EditorPage
import Evergreen.V5.Id
import Evergreen.V5.Keyboard
import Evergreen.V5.Match
import Evergreen.V5.MatchPage
import Evergreen.V5.NonemptySet
import Evergreen.V5.PingData
import Evergreen.V5.Point2d
import Evergreen.V5.Route
import Evergreen.V5.Size
import Evergreen.V5.Sounds
import Evergreen.V5.Textures
import Evergreen.V5.Timeline
import Evergreen.V5.User
import Length
import Pixels
import Quantity
import SeqDict
import Url


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Evergreen.V5.Keyboard.Msg
    | WindowResized Evergreen.V5.Size.Size
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V5.MatchPage.WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | SoundLoaded String (Result Evergreen.V5.Audio.LoadError Evergreen.V5.Audio.Source)
    | TextureLoaded String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    | MatchPageMsg Evergreen.V5.MatchPage.Msg
    | GotTime Effect.Time.Posix
    | RandomInput Effect.Time.Posix
    | EditorPageMsg Evergreen.V5.EditorPage.Msg
    | RejoinMatchTimedOut (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId)


type alias MainLobbyInitData =
    { lobbies : SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId) Evergreen.V5.Match.LobbyPreview
    }


type alias FrontendLoading =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V5.Size.Size
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V5.MatchPage.WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V5.Id.Id Evergreen.V5.User.UserId, MainLobbyInitData )
    , sounds : SeqDict.SeqDict String (Result Evergreen.V5.Audio.LoadError Evergreen.V5.Audio.Source)
    , textures : SeqDict.SeqDict String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    , route : Evergreen.V5.Route.Route
    }


type JoinLobbyError
    = MatchNotFound
    | MatchFull


type alias MainLobbyPage_ =
    { lobbies : SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId) Evergreen.V5.Match.LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage Evergreen.V5.MatchPage.Model
    | EditorPage Evergreen.V5.EditorPage.Model


type alias FrontendLoaded =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V5.Size.Size
    , currentKeys : List Evergreen.V5.Keyboard.Key
    , previousKeys : List Evergreen.V5.Keyboard.Key
    , currentMouse : Evergreen.V5.MatchPage.Mouse
    , previousMouse : Evergreen.V5.MatchPage.Mouse
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V5.MatchPage.WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V5.Sounds.Sounds
    , textures : Evergreen.V5.Textures.Textures
    , userId : Evergreen.V5.Id.Id Evergreen.V5.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe Evergreen.V5.PingData.PingData
    , route : Evergreen.V5.Route.Route
    , loadMatchError : Maybe Effect.Time.Posix
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V5.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        SeqDict.SeqDict
            Effect.Lamdera.SessionId
            { clientIds : SeqDict.SeqDict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V5.Id.Id Evergreen.V5.User.UserId
            }
    , users : SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.User.UserId) BackendUserData
    , lobbies : SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId) Evergreen.V5.Match.Match
    , joiningActiveMatch : SeqDict.SeqDict ( Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId, Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId ) (Evergreen.V5.NonemptySet.NonemptySet Effect.Lamdera.ClientId)
    , dummyChange : Float
    , counter : Int
    , playerPositions : SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId) (SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V5.Id.Id Evergreen.V5.User.UserId) (Evergreen.V5.Point2d.Point2d Length.Meters Evergreen.V5.Match.WorldCoordinate)))
    }


type alias FrontendMsg =
    Evergreen.V5.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend Evergreen.V5.MatchPage.ToBackend
    | EditorPageToBackend Evergreen.V5.EditorPage.ToBackend


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V5.Match.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V5.Match.ServerTime


type JoinMatch
    = JoinedLobby Evergreen.V5.Match.Match
    | JoinedActiveMatch Evergreen.V5.Match.Match (Evergreen.V5.Id.Id Evergreen.V5.Timeline.FrameId) Evergreen.V5.Match.MatchState
    | JoinLobbyError JoinLobbyError


type ToFrontend
    = CreateLobbyResponse (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId) Evergreen.V5.Match.Match
    | RemoveLobbyBroadcast (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId)
    | UpdateLobbyBroadcast (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId) Evergreen.V5.Match.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId) Evergreen.V5.Match.LobbyPreview
    | ClientInit (Evergreen.V5.Id.Id Evergreen.V5.User.UserId) MainLobbyInitData
    | JoinLobbyResponse (Evergreen.V5.Id.Id Evergreen.V5.MatchPage.MatchId) JoinMatch
    | PingResponse Evergreen.V5.Match.ServerTime
    | MatchPageToFrontend Evergreen.V5.MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend Evergreen.V5.EditorPage.ToFrontend
