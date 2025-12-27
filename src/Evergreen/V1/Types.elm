module Evergreen.V1.Types exposing (..)

import Browser
import Duration
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V1.Audio
import Evergreen.V1.EditorPage
import Evergreen.V1.Id
import Evergreen.V1.Keyboard
import Evergreen.V1.Match
import Evergreen.V1.MatchPage
import Evergreen.V1.NonemptySet
import Evergreen.V1.PingData
import Evergreen.V1.Point2d
import Evergreen.V1.Route
import Evergreen.V1.Size
import Evergreen.V1.Sounds
import Evergreen.V1.Timeline
import Evergreen.V1.User
import Length
import Pixels
import Quantity
import SeqDict
import Url


type FrontendMsg_
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | KeyMsg Evergreen.V1.Keyboard.Msg
    | WindowResized Evergreen.V1.Size.Size
    | GotDevicePixelRatio (Quantity.Quantity Float (Quantity.Rate Evergreen.V1.MatchPage.WorldPixel Pixels.Pixels))
    | AnimationFrame Effect.Time.Posix
    | PressedCreateLobby
    | PressedOpenLevelEditor
    | SoundLoaded String (Result Evergreen.V1.Audio.LoadError Evergreen.V1.Audio.Source)
    | MatchPageMsg Evergreen.V1.MatchPage.Msg
    | GotTime Effect.Time.Posix
    | RandomInput Effect.Time.Posix
    | EditorPageMsg Evergreen.V1.EditorPage.Msg
    | RejoinMatchTimedOut (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId)


type alias MainLobbyInitData =
    { lobbies : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId) Evergreen.V1.Match.LobbyPreview
    }


type alias FrontendLoading =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V1.Size.Size
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V1.MatchPage.WorldPixel Pixels.Pixels)
    , time : Maybe Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , initData : Maybe ( Evergreen.V1.Id.Id Evergreen.V1.User.UserId, MainLobbyInitData )
    , sounds : SeqDict.SeqDict String (Result Evergreen.V1.Audio.LoadError Evergreen.V1.Audio.Source)
    , route : Evergreen.V1.Route.Route
    }


type JoinLobbyError
    = MatchNotFound
    | MatchFull


type alias MainLobbyPage_ =
    { lobbies : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId) Evergreen.V1.Match.LobbyPreview
    , joinLobbyError : Maybe JoinLobbyError
    }


type Page
    = MainLobbyPage MainLobbyPage_
    | MatchPage Evergreen.V1.MatchPage.Model
    | EditorPage Evergreen.V1.EditorPage.Model


type alias FrontendLoaded =
    { navigationKey : Effect.Browser.Navigation.Key
    , windowSize : Evergreen.V1.Size.Size
    , currentKeys : List Evergreen.V1.Keyboard.Key
    , previousKeys : List Evergreen.V1.Keyboard.Key
    , currentMouse : Evergreen.V1.MatchPage.Mouse
    , previousMouse : Evergreen.V1.MatchPage.Mouse
    , devicePixelRatio : Quantity.Quantity Float (Quantity.Rate Evergreen.V1.MatchPage.WorldPixel Pixels.Pixels)
    , time : Effect.Time.Posix
    , debugTimeOffset : Duration.Duration
    , page : Page
    , sounds : Evergreen.V1.Sounds.Sounds
    , userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
    , pingStartTime : Maybe Effect.Time.Posix
    , pingData : Maybe Evergreen.V1.PingData.PingData
    , route : Evergreen.V1.Route.Route
    , loadMatchError : Maybe Effect.Time.Posix
    }


type FrontendModel_
    = Loading FrontendLoading
    | Loaded FrontendLoaded


type alias FrontendModel =
    Evergreen.V1.Audio.Model FrontendMsg_ FrontendModel_


type alias BackendUserData =
    { name : String
    }


type alias BackendModel =
    { userSessions :
        SeqDict.SeqDict
            Effect.Lamdera.SessionId
            { clientIds : SeqDict.SeqDict Effect.Lamdera.ClientId ()
            , userId : Evergreen.V1.Id.Id Evergreen.V1.User.UserId
            }
    , users : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) BackendUserData
    , lobbies : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId) Evergreen.V1.Match.Match
    , joiningActiveMatch : SeqDict.SeqDict ( Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId, Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId ) (Evergreen.V1.NonemptySet.NonemptySet Effect.Lamdera.ClientId)
    , dummyChange : Float
    , counter : Int
    , playerPositions : SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId) (SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId) (SeqDict.SeqDict (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) (Evergreen.V1.Point2d.Point2d Length.Meters Evergreen.V1.Match.WorldCoordinate)))
    }


type alias FrontendMsg =
    Evergreen.V1.Audio.Msg FrontendMsg_


type ToBackend
    = CreateMatchRequest
    | PingRequest
    | MatchPageToBackend Evergreen.V1.MatchPage.ToBackend
    | EditorPageToBackend Evergreen.V1.EditorPage.ToBackend


type BackendMsg
    = ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnectedWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId Evergreen.V1.Match.ServerTime
    | UpdateFromFrontendWithTime Effect.Lamdera.SessionId Effect.Lamdera.ClientId ToBackend Evergreen.V1.Match.ServerTime


type JoinMatch
    = JoinedLobby Evergreen.V1.Match.Match
    | JoinedActiveMatch Evergreen.V1.Match.Match (Evergreen.V1.Id.Id Evergreen.V1.Timeline.FrameId) Evergreen.V1.Match.MatchState
    | JoinLobbyError JoinLobbyError


type ToFrontend
    = CreateLobbyResponse (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId) Evergreen.V1.Match.Match
    | RemoveLobbyBroadcast (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId)
    | UpdateLobbyBroadcast (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId) Evergreen.V1.Match.LobbyPreview
    | CreateLobbyBroadcast (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId) Evergreen.V1.Match.LobbyPreview
    | ClientInit (Evergreen.V1.Id.Id Evergreen.V1.User.UserId) MainLobbyInitData
    | JoinLobbyResponse (Evergreen.V1.Id.Id Evergreen.V1.MatchPage.MatchId) JoinMatch
    | PingResponse Evergreen.V1.Match.ServerTime
    | MatchPageToFrontend Evergreen.V1.MatchPage.ToFrontend
    | RejoinMainLobby MainLobbyInitData
    | EditorPageToFrontend Evergreen.V1.EditorPage.ToFrontend
