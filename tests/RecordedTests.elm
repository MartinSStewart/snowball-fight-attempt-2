module RecordedTests exposing (checkPlayersInSync, main, setup)

import Audio
import Backend
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Effect.Browser.Dom as Dom
import Effect.Lamdera as Lamdera exposing (ClientId, SessionId)
import Effect.Test as T exposing (DelayInMs, FileUpload(..), HttpRequest, HttpResponse(..), MultipleFilesUpload(..))
import Effect.WebGL.Texture exposing (Texture)
import Frontend
import Id exposing (Id)
import Json.Decode
import Json.Encode
import Length exposing (Meters)
import List.Extra
import List.Nonempty
import Match exposing (MatchState, WorldCoordinate)
import MatchPage exposing (MatchId, MatchLocalOnly(..))
import Point2d exposing (Point2d)
import Route
import SeqDict exposing (SeqDict)
import Sounds
import Test.Html.Query
import Test.Html.Selector
import Textures
import Time
import Timeline exposing (FrameId)
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendModel_(..), FrontendMsg, Page(..), ToBackend, ToFrontend)
import Url exposing (Url)
import User exposing (UserId)


setup : T.ViewerWith (List (T.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel))
setup =
    T.viewerWith tests
        |> T.addTextureWithOptions Textures.textureOptions "/vignette.png"
        |> T.addTextureWithOptions Textures.textureOptions "/video0.png"
        |> T.addTextureWithOptions Textures.textureOptions "/video1.png"
        |> T.addTextureWithOptions Textures.textureOptions "/video2.png"
        |> T.addTextureWithOptions Textures.textureOptions "/bones/base.png"
        |> T.addTextureWithOptions Textures.textureOptions "/bones/shadow.png"
        |> T.addTextureWithOptions Textures.textureOptions "/charlotte/base.png"
        |> T.addTextureWithOptions Textures.textureOptions "/charlotte/shadow.png"
        |> T.addBytesFiles (Dict.values fileRequests)


main : Program () (T.Model ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel) (T.Msg ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
main =
    T.startViewer setup


domain : Url
domain =
    { protocol = Url.Http, host = "localhost", port_ = Just 8000, path = "", query = Nothing, fragment = Nothing }


{-| Please don't modify or rename this function
-}
fileRequests : Dict String String
fileRequests =
    []
        |> Dict.fromList


stringToJson : String -> Json.Encode.Value
stringToJson json =
    Result.withDefault Json.Encode.null (Json.Decode.decodeString Json.Decode.value json)


handlePortToJs :
    { currentRequest : T.PortToJs, data : T.Data FrontendModel BackendModel }
    -> Maybe ( String, Json.Decode.Value )
handlePortToJs requestAndData =
    case requestAndData.currentRequest.portName of
        "audioPortToJS" ->
            Nothing

        "martinsstewart_elm_device_pixel_ratio_to_js" ->
            Just ( "martinsstewart_elm_device_pixel_ratio_from_js", Json.Encode.float 1 )

        name ->
            let
                _ =
                    Debug.log "Port not handled" ( name, Json.Encode.encode 0 requestAndData.currentRequest.value )
            in
            Nothing


desktopWindow : { width : number, height : number }
desktopWindow =
    { width = 1000, height = 600 }


mobileWindow : { width : number, height : number }
mobileWindow =
    { width = 400, height = 800 }


sessionId0 : SessionId
sessionId0 =
    Lamdera.sessionIdFromString "sessionId0"


sessionId1 : SessionId
sessionId1 =
    Lamdera.sessionIdFromString "sessionId1"


sessionId2 : SessionId
sessionId2 =
    Lamdera.sessionIdFromString "sessionId2"


startTime : Time.Posix
startTime =
    Time.millisToPosix 1765828691000


dropPrefix : String -> String -> String
dropPrefix prefix text =
    if String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


hasExactText :
    T.FrontendActions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> List String
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
hasExactText user texts =
    user.checkView 100 (Test.Html.Query.has (List.map Test.Html.Selector.exactText texts))


hasText :
    T.FrontendActions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> List String
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
hasText user texts =
    user.checkView 100 (Test.Html.Query.has (List.map Test.Html.Selector.text texts))


hasNotExactText :
    T.FrontendActions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> List String
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
hasNotExactText user texts =
    user.checkView 100 (Test.Html.Query.hasNot (List.map Test.Html.Selector.exactText texts))


hasNotText :
    T.FrontendActions ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
    -> List String
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
hasNotText user texts =
    user.checkView 100 (Test.Html.Query.hasNot (List.map Test.Html.Selector.text texts))


tests :
    Texture
    -> Texture
    -> Texture
    -> Texture
    -> Texture
    -> Texture
    -> Texture
    -> Texture
    -> Dict String Bytes
    -> List (T.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests vignette video0 video1 video2 bonesBase bonesShadow charlotteBase charlotteShadow fileData =
    let
        handleHttpRequests : Dict String String -> { currentRequest : HttpRequest, data : T.Data FrontendModel BackendModel } -> HttpResponse
        handleHttpRequests overrides requestAndData =
            let
                okMetadata =
                    { url = requestAndData.currentRequest.url, statusCode = 200, statusText = "OK", headers = Dict.empty }

                key : String
                key =
                    requestAndData.currentRequest.method ++ "_" ++ requestAndData.currentRequest.url

                getData : String -> HttpResponse
                getData path =
                    case Dict.get path fileData of
                        Just data ->
                            BytesHttpResponse okMetadata data

                        Nothing ->
                            UnhandledHttpRequest
            in
            case key of
                "GET_/vignette.png" ->
                    TextureHttpResponse okMetadata vignette

                "GET_/video0.png" ->
                    TextureHttpResponse okMetadata video0

                "GET_/video1.png" ->
                    TextureHttpResponse okMetadata video1

                "GET_/video2.png" ->
                    TextureHttpResponse okMetadata video2

                "GET_/bones/base.png" ->
                    TextureHttpResponse okMetadata bonesBase

                "GET_/bones/shadow.png" ->
                    TextureHttpResponse okMetadata bonesShadow

                "GET_/charlotte/base.png" ->
                    TextureHttpResponse okMetadata charlotteBase

                "GET_/charlotte/shadow.png" ->
                    TextureHttpResponse okMetadata charlotteShadow

                _ ->
                    case ( Dict.get key overrides, Dict.get key fileRequests ) of
                        ( Just path, _ ) ->
                            getData path

                        ( Nothing, Just path ) ->
                            getData path

                        _ ->
                            UnhandledHttpRequest

        config =
            T.Config
                Frontend.app_
                Backend.app_
                (handleHttpRequests Dict.empty)
                handlePortToJs
                (\_ -> UnhandledFileUpload)
                (\_ -> UnhandledMultiFileUpload)
                domain
    in
    [ T.start
        "User rejoins game"
        startTime
        config
        [ T.connectFrontend
            100
            sessionId0
            "/"
            desktopWindow
            (\userA ->
                [ handleAudioPorts userA
                , userA.click 500 (Dom.id "createNewMatch")
                , T.connectFrontend
                    100
                    sessionId1
                    "/"
                    desktopWindow
                    (\userB ->
                        [ handleAudioPorts userB
                        , userB.clickLink 500 (Route.encode (Route.InMatchRoute (Id.fromInt 0)))
                        , userA.click 100 (Dom.id "startMatchSetup")
                        , userA.pointerDown 100 (Dom.id "canvas") ( 500, 100 ) []
                        , userA.pointerUp 100 (Dom.id "canvas") ( 500, 100 ) []
                        , userB.pointerDown 100 (Dom.id "canvas") ( 500, 400 ) []
                        , userB.pointerUp 100 (Dom.id "canvas") ( 500, 400 ) []
                        , checkPlayersInSync 5000
                        ]
                    )
                , T.connectFrontend
                    100
                    sessionId1
                    (Route.encode (Route.InMatchRoute (Id.fromInt 0)))
                    desktopWindow
                    (\userB ->
                        [ handleAudioPorts userB
                        , checkPlayersInSync 500
                        ]
                    )
                ]
            )
        ]
    , T.start
        "Owner rejoins game"
        startTime
        config
        [ T.connectFrontend
            100
            sessionId0
            "/"
            desktopWindow
            (\userA ->
                [ handleAudioPorts userA
                , T.connectFrontend
                    100
                    sessionId1
                    "/"
                    desktopWindow
                    (\userB ->
                        [ handleAudioPorts userB
                        , userB.click 500 (Dom.id "createNewMatch")
                        , userA.clickLink 500 (Route.encode (Route.InMatchRoute (Id.fromInt 0)))
                        , userB.click 100 (Dom.id "startMatchSetup")
                        , userA.pointerDown 100 (Dom.id "canvas") ( 500, 100 ) []
                        , userA.pointerUp 100 (Dom.id "canvas") ( 500, 100 ) []
                        , userB.pointerDown 100 (Dom.id "canvas") ( 500, 400 ) []
                        , userB.pointerUp 100 (Dom.id "canvas") ( 500, 400 ) []
                        , checkPlayersInSync 5000
                        ]
                    )
                , T.connectFrontend
                    100
                    sessionId1
                    (Route.encode (Route.InMatchRoute (Id.fromInt 0)))
                    desktopWindow
                    (\userB ->
                        [ handleAudioPorts userB
                        , checkPlayersInSync 500
                        ]
                    )
                ]
            )
        ]
    ]


handleAudioPorts :
    T.FrontendActions toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
    -> T.Action toBackend frontendMsg frontendModel toFrontend backendMsg backendModel
handleAudioPorts user =
    [ user.portEvent 100 "audioPortFromJS" (stringToJson """{"type":2,"samplesPerSecond":48000}""") ]
        ++ List.map
            (\index ->
                user.portEvent
                    10
                    "audioPortFromJS"
                    ("""{"type":1,"requestId":"""
                        ++ String.fromInt index
                        ++ ""","bufferId":"""
                        ++ String.fromInt index
                        ++ ""","durationInSeconds":0.03325}"""
                        |> stringToJson
                    )
            )
            (List.range 0 (List.length Sounds.soundUrls - 1))
        |> T.group


{-| Verifies that all players in active matches are in sync by checking that
each frontend's computed player positions match at the same frame.
-}
checkPlayersInSync :
    DelayInMs
    -> T.Action ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
checkPlayersInSync delay =
    T.checkState delay
        (\data ->
            let
                frontendMatchData : List { matchId : Id MatchId, frameId : Id FrameId, state : MatchState }
                frontendMatchData =
                    SeqDict.toList data.frontends
                        |> List.concatMap
                            (\( clientId, frontendModel ) ->
                                case Audio.getUserModel frontendModel of
                                    Loaded loaded ->
                                        case loaded.page of
                                            MatchPage matchPage ->
                                                case matchPage.matchData of
                                                    MatchActiveLocal activeLocal ->
                                                        case activeLocal.timelineCache of
                                                            Ok cache ->
                                                                List.map
                                                                    (\( frameId, state ) ->
                                                                        { matchId = matchPage.lobbyId
                                                                        , frameId = frameId
                                                                        , state = state
                                                                        }
                                                                    )
                                                                    (List.Nonempty.toList cache.cache)

                                                            Err _ ->
                                                                []

                                                    MatchSetupLocal _ ->
                                                        []

                                                    MatchError ->
                                                        []

                                            _ ->
                                                []

                                    Loading _ ->
                                        []
                            )
            in
            if
                List.Extra.gatherEqualsBy (\a -> ( a.matchId, a.frameId )) frontendMatchData
                    |> List.all (\( head, rest ) -> List.all (\a -> a == head) rest)
            then
                Ok ()

            else
                Err "Players are no longer in sync"
        )
