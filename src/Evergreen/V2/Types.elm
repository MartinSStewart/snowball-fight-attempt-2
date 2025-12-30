module Evergreen.V2.Types exposing (..)

import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Effect.WebGL.Texture
import Evergreen.V2.Audio
import Evergreen.V2.EditorPage
import Evergreen.V2.Id
import Evergreen.V2.Keyboard
import Evergreen.V2.Match
import Evergreen.V2.MatchPage
import Evergreen.V2.NonemptySet
import Evergreen.V2.PingData
import Evergreen.V2.Point2d
import Evergreen.V2.Route
import Evergreen.V2.Size
import Evergreen.V2.Sounds
import Evergreen.V2.Textures
import Evergreen.V2.Timeline
import Evergreen.V2.User
import Length
import Pixels
import Quantity
import SeqDict
import Url


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Evergreen.V2.Keyboard.Msg
    | WindowResized Evergreen.V2.Size.Size
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V2.MatchPage.WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | SoundLoaded String (Result Evergreen.V2.Audio.LoadError Evergreen.V2.Audio.Source)
    | TextureLoaded String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    | MatchPageMsg Evergreen.V2.MatchPage.Msg
    | GotTime Effect.Time.Posix
    | RandomInput Effect.Time.Posix
    | EditorPageMsg Evergreen.V2.EditorPage.Msg
    | RejoinMatchTimedOut (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId)


type alias MainLobbyInitData =
    { lobbies : SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId) Evergreen.V2.Match.LobbyPreview
    }


type alias FrontendLoading =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V2.Size.Size
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V2.MatchPage.WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V2.Id.Id Evergreen.V2.User.UserId, MainLobbyInitData )
    , sounds : SeqDict.SeqDict String (Result Evergreen.V2.Audio.LoadError Evergreen.V2.Audio.Source)
    , textures : SeqDict.SeqDict String (Result Effect.WebGL.Texture.Error Effect.WebGL.Texture.Texture)
    , route : Evergreen.V2.Route.Route
    }


type JoinLobbyError
    = MatchNotFound
    | MatchFull


type alias MainLobbyPage_ =
    { lobbies : SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId) Evergreen.V2.Match.LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage Evergreen.V2.MatchPage.Model
    | EditorPage Evergreen.V2.EditorPage.Model


type alias FrontendLoaded =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V2.Size.Size
    , currentKeys : List Evergreen.V2.Keyboard.Key
    , previousKeys : List Evergreen.V2.Keyboard.Key
    , currentMouse : Evergreen.V2.MatchPage.Mouse
    , previousMouse : Evergreen.V2.MatchPage.Mouse
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V2.MatchPage.WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V2.Sounds.Sounds
    , textures : Evergreen.V2.Textures.Textures
    , userId : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe Evergreen.V2.PingData.PingData
    , route : Evergreen.V2.Route.Route
    , loadMatchError : Maybe Effect.Time.Posix
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V2.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        SeqDict.SeqDict
            Effect.Lamdera.SessionId
            { clientIds : SeqDict.SeqDict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V2.Id.Id Evergreen.V2.User.UserId
            }
    , users : SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) BackendUserData
    , lobbies : SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId) Evergreen.V2.Match.Match
    , joiningActiveMatch : SeqDict.SeqDict ( Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId, Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId ) (Evergreen.V2.NonemptySet.NonemptySet Effect.Lamdera.ClientId)
    , dummyChange : Float
    , counter : Int
    , playerPositions : SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId) (SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) (Evergreen.V2.Point2d.Point2d Length.Meters Evergreen.V2.Match.WorldCoordinate)))
    }


type alias FrontendMsg =
    Evergreen.V2.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend Evergreen.V2.MatchPage.ToBackend
    | EditorPageToBackend Evergreen.V2.EditorPage.ToBackend


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V2.Match.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V2.Match.ServerTime


type JoinMatch
    = JoinedLobby Evergreen.V2.Match.Match
    | JoinedActiveMatch Evergreen.V2.Match.Match (Evergreen.V2.Id.Id Evergreen.V2.Timeline.FrameId) Evergreen.V2.Match.MatchState
    | JoinLobbyError JoinLobbyError


type ToFrontend
    = CreateLobbyResponse (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId) Evergreen.V2.Match.Match
    | RemoveLobbyBroadcast (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId)
    | UpdateLobbyBroadcast (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId) Evergreen.V2.Match.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId) Evergreen.V2.Match.LobbyPreview
    | ClientInit (Evergreen.V2.Id.Id Evergreen.V2.User.UserId) MainLobbyInitData
    | JoinLobbyResponse (Evergreen.V2.Id.Id Evergreen.V2.MatchPage.MatchId) JoinMatch
    | PingResponse Evergreen.V2.Match.ServerTime
    | MatchPageToFrontend Evergreen.V2.MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend Evergreen.V2.EditorPage.ToFrontend
