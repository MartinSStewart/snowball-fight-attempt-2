module Evergreen.V3.Types exposing (..)

import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL.Texture
import Evergreen.V3.Audio
import Evergreen.V3.EditorPage
import Evergreen.V3.Id
import Evergreen.V3.Keyboard
import Evergreen.V3.Match
import Evergreen.V3.MatchPage
import Evergreen.V3.NonemptySet
import Evergreen.V3.PingData
import Evergreen.V3.Point2d
import Evergreen.V3.Route
import Evergreen.V3.Size
import Evergreen.V3.Sounds
import Evergreen.V3.Textures
import Evergreen.V3.Timeline
import Evergreen.V3.User
import Length
import Pixels
import Quantity
import SeqDict
import Url


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Evergreen.V3.Keyboard.Msg
    | WindowResized Evergreen.V3.Size.Size
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V3.MatchPage.WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | SoundLoaded String (Result Evergreen.V3.Audio.LoadError Evergreen.V3.Audio.Source)
    | TextureLoaded String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    | MatchPageMsg Evergreen.V3.MatchPage.Msg
    | GotTime Effect.Time.Posix
    | RandomInput Effect.Time.Posix
    | EditorPageMsg Evergreen.V3.EditorPage.Msg
    | RejoinMatchTimedOut (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId)


type alias MainLobbyInitData =
    { lobbies : SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId) Evergreen.V3.Match.LobbyPreview
    }


type alias FrontendLoading =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V3.Size.Size
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V3.MatchPage.WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V3.Id.Id Evergreen.V3.User.UserId, MainLobbyInitData )
    , sounds : SeqDict.SeqDict String (Result Evergreen.V3.Audio.LoadError Evergreen.V3.Audio.Source)
    , textures : SeqDict.SeqDict String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    , route : Evergreen.V3.Route.Route
    }


type JoinLobbyError
    = MatchNotFound
    | MatchFull


type alias MainLobbyPage_ =
    { lobbies : SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId) Evergreen.V3.Match.LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage Evergreen.V3.MatchPage.Model
    | EditorPage Evergreen.V3.EditorPage.Model


type alias FrontendLoaded =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V3.Size.Size
    , currentKeys : List Evergreen.V3.Keyboard.Key
    , previousKeys : List Evergreen.V3.Keyboard.Key
    , currentMouse : Evergreen.V3.MatchPage.Mouse
    , previousMouse : Evergreen.V3.MatchPage.Mouse
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V3.MatchPage.WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V3.Sounds.Sounds
    , textures : Evergreen.V3.Textures.Textures
    , userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe Evergreen.V3.PingData.PingData
    , route : Evergreen.V3.Route.Route
    , loadMatchError : Maybe Effect.Time.Posix
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V3.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        SeqDict.SeqDict
            Effect.Lamdera.SessionId
            { clientIds : SeqDict.SeqDict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V3.Id.Id Evergreen.V3.User.UserId
            }
    , users : SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) BackendUserData
    , lobbies : SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId) Evergreen.V3.Match.Match
    , joiningActiveMatch : SeqDict.SeqDict ( Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId, Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId ) (Evergreen.V3.NonemptySet.NonemptySet Effect.Lamdera.ClientId)
    , dummyChange : Float
    , counter : Int
    , playerPositions : SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId) (SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) (Evergreen.V3.Point2d.Point2d Length.Meters Evergreen.V3.Match.WorldCoordinate)))
    }


type alias FrontendMsg =
    Evergreen.V3.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend Evergreen.V3.MatchPage.ToBackend
    | EditorPageToBackend Evergreen.V3.EditorPage.ToBackend


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V3.Match.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V3.Match.ServerTime


type JoinMatch
    = JoinedLobby Evergreen.V3.Match.Match
    | JoinedActiveMatch Evergreen.V3.Match.Match (Evergreen.V3.Id.Id Evergreen.V3.Timeline.FrameId) Evergreen.V3.Match.MatchState
    | JoinLobbyError JoinLobbyError


type ToFrontend
    = CreateLobbyResponse (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId) Evergreen.V3.Match.Match
    | RemoveLobbyBroadcast (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId)
    | UpdateLobbyBroadcast (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId) Evergreen.V3.Match.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId) Evergreen.V3.Match.LobbyPreview
    | ClientInit (Evergreen.V3.Id.Id Evergreen.V3.User.UserId) MainLobbyInitData
    | JoinLobbyResponse (Evergreen.V3.Id.Id Evergreen.V3.MatchPage.MatchId) JoinMatch
    | PingResponse Evergreen.V3.Match.ServerTime
    | MatchPageToFrontend Evergreen.V3.MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend Evergreen.V3.EditorPage.ToFrontend
