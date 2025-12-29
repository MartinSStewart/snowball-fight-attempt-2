module MatchPage exposing
    ( MatchId
    , MatchLocalOnly(..)
    , Model
    , Mouse
    , Msg
    , ScreenCoordinate
    , ToBackend(..)
    , ToFrontend(..)
    , WorldPixel
    , actualTime
    , animationFrame
    , audio
    , backgroundGrid
    , camera
    , canvasView
    , fragmentShader
    , init
    , lineMesh
    , screenToWorld
    , update
    , updateFromBackend
    , validateBotCount
    , vertexShader
    , view
    )

import Acceleration exposing (Acceleration, MetersPerSecondSquared)
import Angle exposing (Angle)
import Array exposing (Array)
import Audio exposing (Audio)
import Axis2d
import Axis3d
import Camera3d exposing (Camera3d)
import Character exposing (Character)
import ColorIndex exposing (ColorIndex(..))
import Decal exposing (Decal)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Direction3d
import Duration exposing (Duration)
import Ease exposing (Easing)
import Effect.Browser.Dom as Dom exposing (HtmlId)
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Task as Task
import Effect.Time as Time
import Effect.WebGL as WebGL exposing (Mesh, Shader)
import Effect.WebGL.Texture exposing (Texture)
import Env
import FontRender
import Geometry
import Geometry.Interop.LinearAlgebra.Point2d
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer
import Id exposing (Id)
import Json.Decode
import Keyboard exposing (Key)
import KeyboardExtra as Keyboard
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import Match exposing (Action(..), Emote(..), Input, LobbyPreview, Match, MatchActive, MatchState, Particle, Player, PlayerData, PlayerMode(..), ServerTime(..), Snowball, Team(..), TextureVertex, TimelineEvent, Vertex, Winner(..), WorldCoordinate)
import MatchName exposing (MatchName)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import MyUi
import NetworkModel exposing (EventId, NetworkModel)
import PingData exposing (PingData)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity(..), Rate)
import Random
import Random.Extra
import Random.List as Random
import RasterShapes
import Rectangle2d exposing (Rectangle2d)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Shape exposing (RenderableShape)
import Size exposing (Size)
import SkinTone exposing (SkinTone)
import Sounds exposing (Sounds)
import Speed exposing (MetersPerSecond)
import TextMessage exposing (TextMessage)
import Textures exposing (Textures)
import Timeline exposing (FrameId, TimelineCache, getOldestCachedState)
import TriangularMesh exposing (TriangularMesh)
import Ui
import Ui.Events
import Ui.Font
import Ui.Input
import Ui.Prose
import User exposing (UserId)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Matrices
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest


type Msg
    = PressedStartMatchSetup
    | PressedLeaveMatchSetup
    | PressedPrimaryColor ColorIndex
    | PressedSecondaryColor ColorIndex
    | PressedDecal (Maybe Decal)
    | PressedSkinTone SkinTone
    | PressedCharacter Character.Character
    | TypedMatchName String
    | PressedPlayerMode PlayerMode
    | PressedSaveMatchName MatchName
    | PressedResetMatchName
    | TypedTextMessage String
    | SubmittedTextMessage TextMessage
    | TypedMaxPlayers String
    | PressedSaveMaxPlayers Int
    | PressedResetMaxPlayers
    | ScrolledToBottom
    | PointerDown Html.Events.Extra.Pointer.Event
    | PointerUp Html.Events.Extra.Pointer.Event
    | PointerLeave Html.Events.Extra.Pointer.Event
    | PointerMoved Html.Events.Extra.Pointer.Event
    | PressedLeaveMatch
    | TypedBotCount String


type MatchId
    = LobbyId Never


type MatchLocalOnly
    = MatchSetupLocal MatchSetupLocal_
    | MatchActiveLocal MatchActiveLocal_
    | MatchError


type alias Model =
    { lobbyId : Id MatchId
    , networkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
    , matchData : MatchLocalOnly
    }


init : Id MatchId -> Match -> Maybe ( Id FrameId, MatchState ) -> ( Model, Command FrontendOnly toMsg Msg )
init lobbyId lobby maybeCache =
    let
        networkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
        networkModel =
            NetworkModel.init lobby
    in
    ( { lobbyId = lobbyId
      , networkModel = networkModel
      , matchData =
            case ( Match.matchActive lobby, maybeCache ) of
                ( Just { startTime, timeline }, Just cache ) ->
                    initMatchData startTime (Match.allUsersAndBots lobby) (Just cache)
                        |> MatchActiveLocal

                ( Nothing, Nothing ) ->
                    initMatchSetupData lobby |> MatchSetupLocal

                _ ->
                    MatchError
      }
    , scrollToBottom
    )


type alias MatchSetupLocal_ =
    { matchName : String, message : String, maxPlayers : String, botCount : String }


type ScreenCoordinate
    = ScreenCoordinate Never


type alias MatchActiveLocal_ =
    { timelineCache : Result Timeline.Error (TimelineCache MatchState)
    , userIds : SeqDict (Id UserId) { skinTone : SkinTone, character : Character }
    , wallMesh : Mesh Vertex
    , touchPosition : Maybe (Point2d Pixels ScreenCoordinate)
    , previousTouchPosition : Maybe (Point2d Pixels ScreenCoordinate)
    , primaryDown : Maybe Time.Posix
    , previousPrimaryDown : Maybe Time.Posix
    , desyncedAtFrame : Maybe (Id FrameId)
    , footstepMesh : List (Mesh Vertex)
    }


type ToBackend
    = MatchRequest (Id MatchId) (Id EventId) Match.Msg
    | DesyncCheckRequest (Id MatchId) (Id Timeline.FrameId) (SeqDict (Id UserId) (Point2d Meters WorldCoordinate))
    | CurrentCache (Id MatchId) (Id FrameId) MatchState


type ToFrontend
    = MatchSetupBroadcast (Id MatchId) (Id UserId) Match.Msg
    | MatchSetupResponse (Id MatchId) (Id UserId) Match.Msg (Id EventId)
    | DesyncBroadcast (Id MatchId) (Id FrameId)
    | NeedCurrentCacheBroadcast (Id MatchId) (Id FrameId)


update : Config a -> Msg -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
update config msg model =
    case msg of
        PressedStartMatchSetup ->
            matchSetupUpdate config.userId (Match.StartMatch (timeToServerTime config)) model

        PressedLeaveMatchSetup ->
            matchSetupUpdate config.userId Match.LeaveMatchSetup model

        PressedPrimaryColor colorIndex ->
            matchSetupUpdate config.userId (Match.SetPrimaryColor colorIndex) model

        PressedSecondaryColor colorIndex ->
            matchSetupUpdate config.userId (Match.SetSecondaryColor colorIndex) model

        PressedDecal decal ->
            matchSetupUpdate config.userId (Match.SetDecal decal) model

        PressedSkinTone skinTone ->
            matchSetupUpdate config.userId (Match.SetSkinTone skinTone) model

        PressedCharacter character ->
            matchSetupUpdate config.userId (Match.SetCharacter character) model

        PressedPlayerMode mode ->
            matchSetupUpdate config.userId (Match.SetPlayerMode mode) model

        TypedMatchName matchName ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData | matchName = matchName } |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PressedSaveMatchName matchName ->
            matchSetupUpdate config.userId (Match.SetMatchName matchName) model

        PressedResetMatchName ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData
                                | matchName =
                                    Match.name (getLocalState model)
                                        |> MatchName.toString
                            }
                                |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        TypedTextMessage text ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData | message = text } |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        SubmittedTextMessage message ->
            matchSetupUpdate
                config.userId
                (Match.SendTextMessage message)
                { model
                    | matchData =
                        case model.matchData of
                            MatchActiveLocal _ ->
                                model.matchData

                            MatchSetupLocal matchSetupData ->
                                { matchSetupData | message = "" } |> MatchSetupLocal

                            MatchError ->
                                MatchError
                }
                |> Tuple.mapSecond (\cmd -> Command.batch [ cmd, scrollToBottom ])

        TypedMaxPlayers maxPlayersText ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData | maxPlayers = maxPlayersText } |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PressedSaveMaxPlayers maxPlayers ->
            matchSetupUpdate config.userId (Match.SetMaxPlayers maxPlayers) model

        PressedResetMaxPlayers ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal _ ->
                            model.matchData

                        MatchSetupLocal matchSetupData ->
                            { matchSetupData
                                | maxPlayers =
                                    Match.maxPlayers (getLocalState model)
                                        |> String.fromInt
                            }
                                |> MatchSetupLocal

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PointerDown event ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            if event.isPrimary then
                                { matchData
                                    | touchPosition = Point2d.fromTuple Pixels.pixels event.pointer.clientPos |> Just
                                    , primaryDown = Just (actualTime config)
                                }
                                    |> MatchActiveLocal

                            else
                                model.matchData

                        MatchSetupLocal _ ->
                            model.matchData

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PointerUp event ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            if event.isPrimary then
                                { matchData
                                    | touchPosition = Point2d.fromTuple Pixels.pixels event.pointer.clientPos |> Just
                                    , primaryDown = Nothing
                                }
                                    |> MatchActiveLocal

                            else
                                model.matchData

                        MatchSetupLocal _ ->
                            model.matchData

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PointerLeave event ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            if event.isPrimary then
                                { matchData | touchPosition = Nothing } |> MatchActiveLocal

                            else
                                model.matchData

                        MatchSetupLocal _ ->
                            model.matchData

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        PointerMoved event ->
            ( { model
                | matchData =
                    case model.matchData of
                        MatchActiveLocal matchData ->
                            if event.isPrimary then
                                { matchData
                                    | touchPosition = Point2d.fromTuple Pixels.pixels event.pointer.clientPos |> Just
                                }
                                    |> MatchActiveLocal

                            else
                                model.matchData

                        MatchSetupLocal _ ->
                            model.matchData

                        MatchError ->
                            MatchError
              }
            , Command.none
            )

        ScrolledToBottom ->
            ( model, Command.none )

        PressedLeaveMatch ->
            matchSetupUpdate config.userId Match.LeaveMatchSetup model

        TypedBotCount text ->
            let
                model2 =
                    { model
                        | matchData =
                            case model.matchData of
                                MatchActiveLocal _ ->
                                    model.matchData

                                MatchSetupLocal matchSetupData ->
                                    { matchSetupData | botCount = text } |> MatchSetupLocal

                                MatchError ->
                                    MatchError
                    }
            in
            case validateBotCount text of
                Ok botCount ->
                    matchSetupUpdate config.userId (Match.SetBotCount botCount) model2

                Err _ ->
                    ( model2, Command.none )


validateBotCount : String -> Result String Int
validateBotCount text =
    case String.toInt text of
        Just int ->
            if int >= 0 then
                if int > 16 then
                    Err "Max 16 bots"

                else
                    Ok int

            else
                Err "Must be an positive integer"

        Nothing ->
            Err "Must be an positive integer"


matchSetupUpdate : Id UserId -> Match.Msg -> Model -> ( Model, Command FrontendOnly ToBackend msg )
matchSetupUpdate userId msg matchSetup =
    let
        { eventId, newNetworkModel } =
            NetworkModel.updateFromUser { userId = userId, msg = msg } matchSetup.networkModel
    in
    ( { matchSetup
        | networkModel = newNetworkModel
        , matchData =
            updateMatchData
                msg
                newNetworkModel
                matchSetup.networkModel
                matchSetup.matchData
      }
    , MatchRequest matchSetup.lobbyId eventId msg |> Effect.Lamdera.sendToBackend
    )


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
updateFromBackend msg matchSetup =
    case msg of
        MatchSetupBroadcast lobbyId userId matchSetupMsg ->
            let
                updateHelper =
                    if lobbyId == matchSetup.lobbyId then
                        let
                            newNetworkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
                            newNetworkModel =
                                NetworkModel.updateFromBackend
                                    Match.matchSetupUpdate
                                    Nothing
                                    { userId = userId, msg = matchSetupMsg }
                                    matchSetup.networkModel
                        in
                        { matchSetup
                            | networkModel = newNetworkModel
                            , matchData =
                                updateMatchData
                                    matchSetupMsg
                                    newNetworkModel
                                    matchSetup.networkModel
                                    matchSetup.matchData
                        }

                    else
                        matchSetup
            in
            case matchSetupMsg of
                Match.SendTextMessage _ ->
                    ( updateHelper, scrollToBottom )

                _ ->
                    ( updateHelper, Command.none )

        MatchSetupResponse lobbyId userId matchSetupMsg eventId ->
            ( if lobbyId == matchSetup.lobbyId then
                let
                    newNetworkModel : NetworkModel { userId : Id UserId, msg : Match.Msg } Match
                    newNetworkModel =
                        NetworkModel.updateFromBackend
                            Match.matchSetupUpdate
                            (Just eventId)
                            { userId = userId, msg = matchSetupMsg }
                            matchSetup.networkModel
                in
                { matchSetup
                    | networkModel = newNetworkModel
                    , matchData =
                        updateMatchData matchSetupMsg newNetworkModel matchSetup.networkModel matchSetup.matchData
                }

              else
                matchSetup
            , Command.none
            )

        DesyncBroadcast lobbyId frameId ->
            ( if lobbyId == matchSetup.lobbyId then
                { matchSetup
                    | matchData =
                        case matchSetup.matchData of
                            MatchActiveLocal matchData ->
                                { matchData | desyncedAtFrame = Just frameId }
                                    |> MatchActiveLocal

                            MatchSetupLocal _ ->
                                matchSetup.matchData

                            MatchError ->
                                MatchError
                }

              else
                matchSetup
            , Command.none
            )

        NeedCurrentCacheBroadcast matchId frameId ->
            let
                networkModel : Match
                networkModel =
                    NetworkModel.localState Match.matchSetupUpdate matchSetup.networkModel
            in
            case ( matchSetup.matchData, Match.matchActive networkModel ) of
                ( MatchActiveLocal matchData, Just matchActive ) ->
                    case matchData.timelineCache of
                        Ok timelineCache ->
                            case Timeline.getStateAt gameUpdate frameId timelineCache matchActive.timeline of
                                Ok ( _, ok ) ->
                                    ( matchSetup
                                    , CurrentCache matchId frameId { ok | footsteps = [], mergedFootsteps = [] }
                                        |> Effect.Lamdera.sendToBackend
                                    )

                                Err _ ->
                                    ( matchSetup, Command.none )

                        Err error ->
                            ( matchSetup, Command.none )

                _ ->
                    ( matchSetup, Command.none )


type alias Config a =
    { a
        | windowSize : Size
        , userId : Id UserId
        , pingData : Maybe PingData
        , time : Time.Posix
        , debugTimeOffset : Duration
        , sounds : Sounds
        , previousKeys : List Key
        , currentKeys : List Key
        , previousMouse : Mouse
        , currentMouse : Mouse
        , devicePixelRatio : Quantity Float (Rate WorldPixel Pixels)
        , textures : Textures
    }


type alias Mouse =
    { position : Point2d Pixels ScreenCoordinate
    , primaryDown : Bool
    , secondaryDown : Bool
    }


type WorldPixel
    = WorldPixel Never



--characterView : MatchActiveLocal_ -> MatchState -> List WebGL.Entity
--characterView match state =
--    SeqDict.foldr
--        (\userId player ( redTeam, blueTeam ) ->
--            case SeqDict.get userId match.userIds of
--                Just data ->
--                    case player.team of
--                        RedTeam ->
--                            ( characterViewHelper :: redTeam, blueTeam )
--
--                        BlueTeam ->
--                            ( redTeam, characterViewHelper :: blueTeam )
--
--                Nothing ->
--                    ( redTeam, blueTeam )
--        )
--        ( [], [] )
--        state.players
--        |> (\( a, b ) -> a ++ b)


characterViewHelper : Point2d Pixels ScreenCoordinate -> String -> Ui.Element msg
characterViewHelper position name =
    let
        { x, y } =
            Point2d.toPixels position
    in
    Ui.image
        [ Ui.move { x = round x, y = round y, z = 0 }
        ]
        { source = "/" ++ name ++ "/base.png", description = "", onLoad = Nothing }


view : Config a -> Model -> Ui.Element Msg
view config model =
    let
        lobby : Match
        lobby =
            getLocalState model
    in
    case ( Match.matchActive lobby, model.matchData, Match.allUsers_ lobby |> SeqDict.get config.userId ) of
        ( Just match, MatchActiveLocal matchData, _ ) ->
            case matchData.timelineCache of
                Ok cache ->
                    case Timeline.getStateAt gameUpdate (timeToFrameId config match) cache match.timeline of
                        Ok ( _, matchState ) ->
                            Ui.el
                                (Ui.height Ui.fill
                                    :: Ui.htmlAttribute (Html.Events.Extra.Pointer.onDown PointerDown)
                                    :: Ui.htmlAttribute (Html.Events.Extra.Pointer.onUp PointerUp)
                                    :: Ui.htmlAttribute (Html.Events.Extra.Pointer.onLeave PointerLeave)
                                    :: Ui.id "canvas"
                                    :: Ui.inFront (desyncWarning matchData.desyncedAtFrame)
                                    --:: Ui.inFront
                                    --    (Ui.el
                                    --        [ Ui.width Ui.shrink
                                    --        , Ui.background (Ui.rgb 255 255 255)
                                    --        , Ui.Input.button PressedLeaveMatch
                                    --        ]
                                    --        (Ui.text "Leave match")
                                    --    )
                                    :: Ui.behindContent
                                        (canvasView
                                            config.time
                                            config.windowSize
                                            config.devicePixelRatio
                                            config.textures
                                            (canvasViewHelper config model)
                                        )
                                    :: (case matchData.touchPosition of
                                            Just _ ->
                                                [ Ui.htmlAttribute (Html.Events.Extra.Pointer.onMove PointerMoved) ]

                                            Nothing ->
                                                []
                                       )
                                )
                                Ui.none

                        Err _ ->
                            Ui.text "An error occurred during the match :("

                Err _ ->
                    Ui.text "An error occurred during the match :("

        ( Nothing, MatchSetupLocal matchSetupData, Just currentPlayerData ) ->
            matchSetupView config lobby matchSetupData currentPlayerData

        _ ->
            Ui.text "Loading..."


matchSetupView : Config a -> Match -> MatchSetupLocal_ -> PlayerData -> Ui.Element Msg
matchSetupView config lobby matchSetupData currentPlayerData =
    let
        displayType =
            MyUi.displayType config.windowSize

        matchName : String
        matchName =
            MatchName.toString (Match.name lobby)

        users : List ( Id UserId, PlayerData )
        users =
            Match.allUsers lobby |> List.Nonempty.toList

        preview =
            Match.preview lobby
    in
    Ui.column
        [ Ui.spacing 8
        , Ui.padding (MyUi.ifMobile displayType 8 16)
        , Ui.widthMax 800
        , Ui.height Ui.fill
        ]
        [ if Match.isOwner config.userId lobby then
            Ui.row
                [ Ui.spacing 8 ]
                (Ui.Input.text
                    [ Ui.padding 4
                    , if matchSetupData.matchName == "" then
                        Ui.Font.italic

                      else
                        Ui.noAttr
                    ]
                    { onChange = TypedMatchName
                    , text = matchSetupData.matchName
                    , placeholder = Just "Unnamed match"
                    , label = Ui.Input.labelHidden "Match name"
                    }
                    :: (if matchSetupData.matchName == matchName then
                            []

                        else
                            MyUi.simpleButton (Dom.id "resetMatchName") PressedResetMatchName (Ui.text "Reset")
                                :: (case MatchName.fromString matchSetupData.matchName of
                                        Ok matchName_ ->
                                            [ MyUi.simpleButton (Dom.id "saveMatchName") (PressedSaveMatchName matchName_) (Ui.text "Save") ]

                                        _ ->
                                            []
                                   )
                       )
                )

          else
            Ui.row [ Ui.width Ui.shrink, Ui.Font.bold ]
                [ Ui.text "Match: "
                , if matchName == "" then
                    Ui.el [ Ui.Font.italic ] (Ui.text "Unnamed match")

                  else
                    Ui.text matchName
                ]
        , if Match.isOwner config.userId lobby then
            let
                label =
                    Ui.Input.label "maxPlayers" [ Ui.width Ui.shrink ] (Ui.text "Max players")
            in
            Ui.row
                [ Ui.width Ui.shrink, Ui.spacing 8 ]
                (Ui.row
                    [ Ui.spacing 4 ]
                    [ label.element
                    , Ui.Input.text
                        [ Ui.width (Ui.px 50), Ui.padding 4, Ui.Font.alignRight ]
                        { onChange = TypedMaxPlayers
                        , text = matchSetupData.maxPlayers
                        , placeholder = Nothing
                        , label = label.id
                        }
                    ]
                    :: (if matchSetupData.maxPlayers == String.fromInt preview.maxUserCount then
                            []

                        else
                            MyUi.simpleButton (Dom.id "resetMaxPlayers") PressedResetMaxPlayers (Ui.text "Reset")
                                :: (case String.toInt matchSetupData.maxPlayers of
                                        Just maxPlayers ->
                                            [ MyUi.simpleButton (Dom.id "saveMaxPlayers") (PressedSaveMaxPlayers maxPlayers) (Ui.text "Save") ]

                                        Nothing ->
                                            []
                                   )
                       )
                )

          else
            Ui.none
        , if Match.isOwner config.userId lobby then
            let
                label =
                    Ui.Input.label "maxPlayers" [ Ui.width Ui.shrink ] (Ui.text "Number of bots")
            in
            Ui.row
                [ Ui.width Ui.shrink, Ui.spacing 8 ]
                [ Ui.row
                    [ Ui.spacing 4 ]
                    [ label.element
                    , Ui.Input.text
                        [ Ui.width (Ui.px 50), Ui.padding 4, Ui.Font.alignRight ]
                        { onChange = TypedBotCount
                        , text = matchSetupData.botCount
                        , placeholder = Nothing
                        , label = label.id
                        }
                    ]
                , case validateBotCount matchSetupData.botCount of
                    Ok _ ->
                        Ui.none

                    Err error ->
                        Ui.text error
                ]

          else
            Ui.none
        , Ui.row
            [ Ui.width Ui.shrink, Ui.spacing 8 ]
            [ if Match.isOwner config.userId lobby then
                MyUi.simpleButton (Dom.id "startMatchSetup") PressedStartMatchSetup (Ui.text "Start match")

              else
                Ui.none
            , MyUi.simpleButton (Dom.id "leaveMatchSetup") PressedLeaveMatchSetup (Ui.text "Leave")
            , case currentPlayerData.mode of
                PlayerMode ->
                    MyUi.simpleButton (Dom.id "switchToSpectator") (PressedPlayerMode SpectatorMode) (Ui.text "Switch to spectator")

                SpectatorMode ->
                    MyUi.simpleButton (Dom.id "switchToPlayer") (PressedPlayerMode PlayerMode) (Ui.text "Switch to player")
            ]
        , Ui.column
            [ Ui.width Ui.shrink, Ui.spacing 8 ]
            [ Ui.column
                [ Ui.width Ui.shrink
                , Ui.spacing 8
                , Ui.opacity
                    (case currentPlayerData.mode of
                        PlayerMode ->
                            1

                        SpectatorMode ->
                            0.5
                    )
                ]
                [ Ui.column
                    [ Ui.width Ui.shrink, Ui.spacing 4, Ui.Font.size 16, Ui.Font.bold ]
                    [ Ui.text "Primary color"
                    , colorSelector PressedPrimaryColor currentPlayerData.primaryColor
                    ]
                , Ui.column
                    [ Ui.width Ui.shrink, Ui.spacing 4, Ui.Font.size 16, Ui.Font.bold ]
                    [ Ui.text "Secondary color"
                    , colorSelector PressedSecondaryColor currentPlayerData.secondaryColor
                    ]
                , Ui.column
                    [ Ui.width Ui.shrink, Ui.spacing 4, Ui.Font.size 16, Ui.Font.bold ]
                    [ Ui.text "Skin tone"
                    , skinToneSelector PressedSkinTone currentPlayerData.skinTone
                    ]
                , Ui.column
                    [ Ui.spacing 4 ]
                    [ Ui.el [ Ui.width Ui.shrink, Ui.Font.size 16, Ui.Font.bold ] (Ui.text "Decal")
                    , Nothing
                        :: List.map Just (List.Nonempty.toList Decal.allDecals)
                        |> List.map
                            (\maybeDecal ->
                                MyUi.button
                                    (decalHtmlId maybeDecal)
                                    [ Ui.paddingXY 4 4
                                    , Ui.background
                                        (if maybeDecal == currentPlayerData.decal then
                                            Ui.rgb 153 179 255

                                         else
                                            Ui.rgb 204 204 204
                                        )
                                    ]
                                    { onPress = PressedDecal maybeDecal
                                    , label =
                                        (case maybeDecal of
                                            Just decal ->
                                                Decal.toString decal

                                            Nothing ->
                                                "None"
                                        )
                                            |> Ui.text
                                    }
                            )
                        |> Ui.row [ Ui.spacing 8 ]
                    ]
                , Ui.column
                    [ Ui.spacing 4 ]
                    [ Ui.el [ Ui.width Ui.shrink, Ui.Font.size 16, Ui.Font.bold ] (Ui.text "Character")
                    , Character.all
                        |> List.map
                            (\character ->
                                MyUi.button
                                    (characterHtmlId character)
                                    [ Ui.paddingXY 8 8
                                    , Ui.background
                                        (if character == currentPlayerData.character then
                                            Ui.rgb 153 179 255

                                         else
                                            Ui.rgb 204 204 204
                                        )
                                    ]
                                    { onPress = PressedCharacter character
                                    , label =
                                        Ui.image
                                            [ Ui.width (Ui.px 100)
                                            , Ui.height (Ui.px 100)
                                            ]
                                            { source = "/bones/base.png"
                                            , description = Character.toString character
                                            , onLoad = Nothing
                                            }
                                    }
                            )
                        |> Ui.row [ Ui.spacing 8, Ui.wrap ]
                    ]
                ]
            ]
        , Ui.column
            [ Ui.spacing 16, Ui.height Ui.fill ]
            [ Ui.column
                [ Ui.width Ui.shrink, Ui.spacing 8, Ui.alignTop, Ui.Font.size 16 ]
                [ Ui.text "Participants:"
                , Ui.column
                    [ Ui.width Ui.shrink ]
                    (List.map
                        (\( userId, playerData ) ->
                            "User "
                                ++ String.fromInt (Id.toInt userId)
                                ++ (case playerData.mode of
                                        PlayerMode ->
                                            ""

                                        SpectatorMode ->
                                            " (spectator)"
                                   )
                                |> Ui.text
                        )
                        users
                        ++ (case Match.botCount lobby of
                                0 ->
                                    []

                                1 ->
                                    [ Ui.text "(and 1 bot)" ]

                                many ->
                                    [ Ui.text ("(and " ++ String.fromInt many ++ " bots)") ]
                           )
                    )
                ]
            , textChat matchSetupData lobby
            ]
        ]


decalHtmlId : Maybe Decal -> HtmlId
decalHtmlId maybeDecal =
    "selectDecal_"
        ++ (case maybeDecal of
                Just decal ->
                    case decal of
                        Decal.Star ->
                            "Star"

                        Decal.Triangle ->
                            "Triangle"

                        Decal.Plus ->
                            "Plus"

                        Decal.Minus ->
                            "Minus"

                        Decal.Square ->
                            "Square"

                        Decal.HollowSquare ->
                            "HollowSquare"

                Nothing ->
                    "noDecal"
           )
        |> Dom.id


characterHtmlId : Character.Character -> HtmlId
characterHtmlId character =
    "selectCharacter_" ++ Character.toString character |> Dom.id


textChat : MatchSetupLocal_ -> Match -> Ui.Element Msg
textChat matchSetupData lobby =
    Ui.column
        [ Ui.scrollable
        , Ui.height Ui.fill
        , Ui.padding 4
        ]
        [ Match.messagesOldestToNewest lobby
            |> List.map
                (\{ userId, message } ->
                    let
                        userName : String
                        userName =
                            Id.toInt userId |> String.fromInt |> (++) "User "
                    in
                    Ui.row
                        [ Ui.width Ui.shrink, Ui.Font.size 16 ]
                        [ (if Match.isOwner userId lobby then
                            userName ++ " (host)" ++ " "

                           else
                            userName ++ " "
                          )
                            |> Ui.text
                            |> Ui.el [ Ui.width Ui.shrink, Ui.Font.bold, Ui.alignTop ]
                        , TextMessage.toString message |> Ui.text |> List.singleton |> Ui.Prose.paragraph [ Ui.width Ui.shrink ]
                        ]
                )
            |> Ui.column
                [ Ui.spacing 4
                , Ui.scrollable
                , Ui.height Ui.fill
                , Ui.paddingXY 0 8
                , Ui.htmlAttribute (Dom.idToAttribute textMessageContainerId)
                ]
        , Ui.Input.text
            (Ui.Font.size 16
                :: Ui.padding 8
                :: (case TextMessage.fromString matchSetupData.message of
                        Ok message ->
                            [ Html.Events.on "keydown"
                                (Json.Decode.field "keyCode" Json.Decode.int
                                    |> Json.Decode.andThen
                                        (\key ->
                                            if key == 13 then
                                                SubmittedTextMessage message |> Json.Decode.succeed

                                            else
                                                Json.Decode.fail ""
                                        )
                                )
                                |> Ui.htmlAttribute
                            ]

                        Err _ ->
                            []
                   )
            )
            { onChange = TypedTextMessage
            , text = matchSetupData.message
            , placeholder = Just "Press enter to send"
            , label = Ui.Input.labelHidden "Write message"
            }
        ]


findPixelPerfectSize :
    Size
    -> Quantity Float (Rate WorldPixel Pixels)
    -> { canvasSize : ( Quantity Int Pixels, Quantity Int Pixels ), actualCanvasSize : Size }
findPixelPerfectSize windowSize (Quantity pixelRatio) =
    let
        findValue : Quantity Int Pixels -> ( Int, Int )
        findValue value =
            List.range 0 9
                |> List.map ((+) (Pixels.inPixels value))
                |> List.find
                    (\v ->
                        let
                            a =
                                toFloat v * pixelRatio
                        in
                        a == toFloat (round a) && modBy 2 (round a) == 0
                    )
                |> Maybe.map (\v -> ( v, toFloat v * pixelRatio |> round ))
                |> Maybe.withDefault ( Pixels.inPixels value, toFloat (Pixels.inPixels value) * pixelRatio |> round )

        ( w, actualW ) =
            findValue windowSize.width

        ( h, actualH ) =
            findValue windowSize.height
    in
    { canvasSize = ( Pixels.pixels w, Pixels.pixels h )
    , actualCanvasSize = { width = Pixels.pixels actualW, height = Pixels.pixels actualH }
    }


canvasView : Time.Posix -> Size -> Quantity Float (Rate WorldPixel Pixels) -> Textures -> (Size -> List WebGL.Entity) -> Ui.Element msg
canvasView time windowSize devicePixelRatio textures entities =
    let
        ( Quantity cssWindowWidth, Quantity cssWindowHeight ) =
            canvasSize

        { canvasSize, actualCanvasSize } =
            findPixelPerfectSize windowSize devicePixelRatio

        videoTexture =
            case Time.posixToMillis time // 10 |> modBy 3 of
                0 ->
                    textures.video0

                1 ->
                    textures.video1

                _ ->
                    textures.video2

        overlayEntities =
            drawOverlays actualCanvasSize textures.vignette videoTexture
    in
    WebGL.toHtmlWith
        [ WebGL.alpha True, WebGL.stencil 0, WebGL.depth 1, WebGL.clearColor 0 0 0 1 ]
        [ Html.Attributes.width (Pixels.inPixels actualCanvasSize.width)
        , Html.Attributes.height (Pixels.inPixels actualCanvasSize.height)
        , Html.Attributes.style "width" (String.fromInt cssWindowWidth ++ "px")
        , Html.Attributes.style "height" (String.fromInt cssWindowHeight ++ "px")
        ]
        (entities actualCanvasSize ++ overlayEntities)
        |> Ui.html
        |> Ui.el []


overlayRatio =
    overlayWidth / overlayHeight


overlayWidth =
    1200


overlayHeight =
    800


drawOverlays : Size -> Texture -> Texture -> List WebGL.Entity
drawOverlays canvasSize vignetteTexture videoTexture =
    let
        screenAspect =
            toFloat (Pixels.inPixels canvasSize.width) / toFloat (Pixels.inPixels canvasSize.height)

        scaleX =
            overlayRatio / screenAspect

        modelMatrix =
            Mat4.makeScale3 scaleX 1 1

        drawOverlay texture =
            WebGL.entityWith
                [ Blend.add Blend.one Blend.oneMinusSrcAlpha ]
                textureVertexShader
                textureFragmentShader
                overlayMesh
                { ucolor = Vec3.vec3 1 1 1
                , view = Mat4.identity
                , model = modelMatrix
                , texture = texture
                }
    in
    [ drawOverlay videoTexture
    , drawOverlay vignetteTexture
    ]


camera : Point2d Meters WorldCoordinate -> Length -> Camera3d Meters WorldCoordinate
camera position viewportHeight2 =
    let
        { x, y } =
            Point2d.toMeters position
    in
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.fromMeters { x = x, y = y, z = 0 }
                , eyePoint = Point3d.fromMeters { x = x, y = y, z = 20 }
                , upDirection = Direction3d.y
                }
        , viewportHeight = viewportHeight2
        }


screenToWorld : Size -> Point2d Meters WorldCoordinate -> Length -> Point2d Pixels ScreenCoordinate -> Point2d Meters WorldCoordinate
screenToWorld windowSize cameraPosition viewportHeight2 screenPosition =
    let
        screenRectangle : Rectangle2d Pixels ScreenCoordinate
        screenRectangle =
            Rectangle2d.from
                (Point2d.xy Quantity.zero (Quantity.toFloatQuantity windowSize.height))
                (Point2d.xy (Quantity.toFloatQuantity windowSize.width) Quantity.zero)
    in
    Camera3d.ray (camera cameraPosition viewportHeight2) screenRectangle screenPosition
        |> Axis3d.originPoint
        |> (\p -> Point3d.toMeters p |> (\a -> Point2d.meters a.x a.y))


backgroundGrid : Point2d units coordinates -> Float -> Size -> WebGL.Entity
backgroundGrid cameraPosition zoom canvasSize =
    let
        { width, height } =
            canvasSize

        canvasWidth =
            Pixels.inPixels width |> toFloat

        canvasHeight =
            Pixels.inPixels height |> toFloat
    in
    WebGL.entityWith
        [ WebGL.Settings.cullFace WebGL.Settings.back ]
        backgroundVertexShader
        backgroundFragmentShader
        squareMesh
        { view = Geometry.Interop.LinearAlgebra.Point2d.toVec2 cameraPosition
        , viewZoom = canvasHeight * zoom
        , windowSize = Vec2.vec2 canvasWidth canvasHeight
        }


countdown : Array.Array RenderableShape
countdown =
    Array.fromList [ Shape.three, Shape.two, Shape.one, Shape.go ]


redNumbers : Array RenderableShape
redNumbers =
    List.map (Shape.setColor (Vec3.vec3 1 0 0)) [ Shape.zero, Shape.one, Shape.two, Shape.three ] |> Array.fromList


blueNumbers : Array RenderableShape
blueNumbers =
    List.map (Shape.setColor (Vec3.vec3 0 0 1)) [ Shape.zero, Shape.one, Shape.two, Shape.three ] |> Array.fromList


drawCountdown : Id FrameId -> Mat4 -> List WebGL.Entity
drawCountdown frameId viewMatrix =
    let
        gameTimeElapsed : Duration
        gameTimeElapsed =
            frameTimeElapsed (Id.fromInt 0) frameId

        elapsedSeconds : Int
        elapsedSeconds =
            Duration.inSeconds gameTimeElapsed |> floor
    in
    case Array.get (elapsedSeconds - 1) countdown of
        Just value ->
            drawShape
                (toFromAndBack
                    (Duration.milliseconds 100)
                    (Duration.seconds 800)
                    (Duration.milliseconds 100)
                    (gameTimeElapsed |> Quantity.minus (Duration.seconds (toFloat elapsedSeconds)))
                    Ease.outBack
                    Ease.inBack
                    0
                    0.01
                )
                Point2d.origin
                viewMatrix
                value

        Nothing ->
            []


drawParticles : Id FrameId -> Mat4 -> List Particle -> List WebGL.Entity
drawParticles frameId viewMatrix particles =
    List.map
        (\particle ->
            let
                timeElapsed =
                    frameTimeElapsed particle.spawnedAt frameId

                { x, y, z } =
                    particle.position
                        |> Point3d.translateBy (Vector2d.for timeElapsed particle.velocity |> vector2To3 Quantity.zero)
                        |> Point3d.toMeters

                t : Float
                t =
                    Quantity.ratio
                        timeElapsed
                        particle.lifetime

                size : Float
                size =
                    Length.inMeters particle.size * (1 - t)
            in
            WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back ]
                vertexShader
                fragmentShader
                particleOutlineMesh
                { ucolor = Vec3.vec3 1 1 1
                , view = viewMatrix
                , model =
                    Mat4.makeTranslate3 x y 1 |> Mat4.scale3 size size size
                }
        )
        particles
        ++ List.map
            (\particle ->
                let
                    timeElapsed =
                        frameTimeElapsed particle.spawnedAt frameId

                    { x, y } =
                        particle.position
                            |> Point3d.translateBy (Vector2d.for timeElapsed particle.velocity |> vector2To3 Quantity.zero)
                            |> Point3d.toMeters

                    t : Float
                    t =
                        Quantity.ratio
                            timeElapsed
                            particle.lifetime

                    size : Float
                    size =
                        Length.inMeters particle.size * (1 - t)
                in
                WebGL.entityWith
                    [ WebGL.Settings.cullFace WebGL.Settings.back ]
                    vertexShader
                    fragmentShader
                    particleMesh
                    { ucolor = Vec3.vec3 1 1 1
                    , view = viewMatrix
                    , model =
                        Mat4.makeTranslate3 x y 1 |> Mat4.scale3 size size size
                    }
            )
            particles


viewWidth =
    18.5


viewHeight =
    14


mapFillMesh : Mesh Vertex
mapFillMesh =
    WebGL.triangleFan
        [ { position = Vec3.vec3 -viewWidth -viewHeight -1, color = Vec3.vec3 1 1 1 }
        , { position = Vec3.vec3 viewWidth -viewHeight -1, color = Vec3.vec3 1 1 1 }
        , { position = Vec3.vec3 viewWidth viewHeight -1, color = Vec3.vec3 1 1 1 }
        , { position = Vec3.vec3 -viewWidth viewHeight -1, color = Vec3.vec3 1 1 1 }
        ]


overlayMesh : Mesh TextureVertex
overlayMesh =
    WebGL.triangleFan
        [ { position = Vec3.vec3 -1 -1 0, uv = Vec2.vec2 0 1 }
        , { position = Vec3.vec3 1 -1 0, uv = Vec2.vec2 1 1 }
        , { position = Vec3.vec3 1 1 0, uv = Vec2.vec2 1 0 }
        , { position = Vec3.vec3 -1 1 0, uv = Vec2.vec2 0 0 }
        ]


drawScoreNumber :
    Team
    -> Mat4
    -> Id FrameId
    -> Int
    -> Maybe { time : Id FrameId, winner : Winner }
    -> List WebGL.Entity
drawScoreNumber team viewMatrix frameId score roundEndTime =
    let
        position =
            case team of
                RedTeam ->
                    Point2d.meters -2 8

                BlueTeam ->
                    Point2d.meters 2 8

        numbers =
            case team of
                RedTeam ->
                    redNumbers

                BlueTeam ->
                    blueNumbers
    in
    case Array.get score numbers of
        Just number ->
            case roundEndTime of
                Just roundEnded ->
                    let
                        elapsed =
                            frameTimeElapsed roundEnded.time frameId |> Quantity.minus Duration.second

                        t =
                            Quantity.ratio elapsed (Duration.seconds 0.2) |> clamp 0 1

                        t0 =
                            t * 2 |> clamp 0 1

                        t1 =
                            (t - 0.5) * 2 |> clamp 0 1
                    in
                    case
                        ( (roundEnded.winner == RedWon && team == RedTeam)
                            || (roundEnded.winner == BlueWon && team == BlueTeam)
                            || (roundEnded.winner == BothWon)
                        , Array.get (score + 1) numbers
                        )
                    of
                        ( False, _ ) ->
                            drawShape 0.004 position viewMatrix number

                        ( True, Just numberPlus1 ) ->
                            drawShape ((1 - t0) * 0.004) position viewMatrix number
                                ++ drawShape (0.004 * t1) position viewMatrix numberPlus1

                        ( True, Nothing ) ->
                            drawShape 0.004 position viewMatrix number

                Nothing ->
                    drawShape 0.004 position viewMatrix number

        Nothing ->
            []


canvasViewHelper : Config a -> Model -> Size -> List WebGL.Entity
canvasViewHelper model matchSetup canvasSize =
    case ( Match.matchActive (getLocalState matchSetup), matchSetup.matchData ) of
        ( Just match, MatchActiveLocal matchData ) ->
            case matchData.timelineCache of
                Ok cache ->
                    let
                        frameId =
                            timeToFrameId model match
                    in
                    case Timeline.getStateAt gameUpdate frameId cache match.timeline of
                        Ok ( _, state ) ->
                            let
                                zoom : Float
                                zoom =
                                    1 / Length.inMeters viewportHeight

                                --zoomFactor
                                --    * toFloat (max canvasWidth canvasHeight)
                                --    / (toFloat canvasHeight * 2000)
                                viewMatrix : Mat4
                                viewMatrix =
                                    WebGL.Matrices.viewProjectionMatrix
                                        (camera Point2d.origin (Length.meters (1 / zoom)))
                                        { nearClipDepth = Length.meters 0.1
                                        , farClipDepth = Length.meters 30
                                        , aspectRatio =
                                            Quantity.ratio
                                                (Quantity.toFloatQuantity canvasSize.width)
                                                (Quantity.toFloatQuantity canvasSize.height)
                                        }
                            in
                            WebGL.entityWith
                                [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                vertexShader
                                fragmentShader
                                mapFillMesh
                                { ucolor = Vec3.vec3 1 1 1
                                , view = viewMatrix
                                , model = Mat4.identity
                                }
                                :: drawCountdown frameId viewMatrix
                                ++ [ WebGL.entityWith
                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                        vertexShader
                                        fragmentShader
                                        matchData.wallMesh
                                        { ucolor = Vec3.vec3 1 1 1
                                        , view = viewMatrix
                                        , model = Mat4.identity
                                        }
                                   ]
                                ++ drawScoreNumber RedTeam viewMatrix frameId state.score.redTeam state.roundEndTime
                                ++ drawScoreNumber BlueTeam viewMatrix frameId state.score.blueTeam state.roundEndTime
                                ++ List.concatMap
                                    (\( userId, player ) ->
                                        drawPlayer
                                            (timeToFrameId model match)
                                            userId
                                            matchData
                                            viewMatrix
                                            player
                                    )
                                    (SeqDict.toList state.players)
                                ++ List.map
                                    (\footstepMesh2 ->
                                        WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                                            vertexShader
                                            fragmentShader
                                            footstepMesh2.mesh
                                            { ucolor = Vec3.vec3 1 1 1
                                            , view = viewMatrix
                                            , model = Mat4.identity
                                            }
                                    )
                                    state.footsteps
                                ++ List.map
                                    (\footstepMesh2 ->
                                        WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                                            vertexShader
                                            fragmentShader
                                            footstepMesh2
                                            { ucolor = Vec3.vec3 1 1 1
                                            , view = viewMatrix
                                            , model = Mat4.identity
                                            }
                                    )
                                    state.mergedFootsteps
                                ++ List.concatMap
                                    (\snowball ->
                                        let
                                            { x, y, z } =
                                                Point3d.toMeters snowball.position

                                            snowballRadius_ =
                                                Length.inMeters snowballRadius
                                        in
                                        [ WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                                            vertexShader
                                            fragmentShader
                                            snowballShadowMesh
                                            { ucolor = Vec3.vec3 1 1 1
                                            , view = viewMatrix
                                            , model =
                                                Mat4.makeTranslate3 x y 0.01
                                                    |> Mat4.scale3 snowballRadius_ snowballRadius_ snowballRadius_
                                            }
                                        , WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                                            vertexShader
                                            fragmentShader
                                            snowballMesh
                                            { ucolor = Vec3.vec3 1 1 1
                                            , view = viewMatrix
                                            , model =
                                                Mat4.makeTranslate3 x (y + z) z
                                                    |> Mat4.scale3 snowballRadius_ snowballRadius_ snowballRadius_
                                            }
                                        ]
                                    )
                                    state.snowballs
                                ++ List.concatMap
                                    (\pushableSnowball ->
                                        let
                                            { x, y } =
                                                Point2d.toMeters pushableSnowball.position

                                            radius =
                                                Length.inMeters pushableSnowball.radius

                                            outlineRadius =
                                                radius + 0.05
                                        in
                                        [ WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                                            vertexShader
                                            fragmentShader
                                            pushableSnowballMesh
                                            { ucolor = Vec3.vec3 0 0 0
                                            , view = viewMatrix
                                            , model =
                                                Mat4.makeTranslate3 x y radius
                                                    |> Mat4.scale3 outlineRadius outlineRadius outlineRadius
                                            }
                                        , WebGL.entityWith
                                            [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                                            vertexShader
                                            fragmentShader
                                            pushableSnowballMesh
                                            { ucolor = Vec3.vec3 1 1 1
                                            , view = viewMatrix
                                            , model =
                                                Mat4.makeTranslate3 x y (radius + 0.01)
                                                    |> Mat4.scale3 radius radius radius
                                            }
                                        ]
                                    )
                                    state.pushableSnowballs
                                ++ drawParticles frameId viewMatrix state.particles
                                ++ (case SeqDict.get model.userId state.players of
                                        Just player ->
                                            case player.clickStart of
                                                Just clickStart ->
                                                    let
                                                        currentFrameId =
                                                            timeToFrameId model match

                                                        elapsed =
                                                            frameTimeElapsed clickStart.time currentFrameId
                                                    in
                                                    if elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay then
                                                        case Direction2d.from player.position clickStart.position of
                                                            Just direction ->
                                                                let
                                                                    targetPosition =
                                                                        Point2d.translateBy
                                                                            (Vector2d.withLength (throwDistance elapsed) direction)
                                                                            player.position

                                                                    reticleScale =
                                                                        0.2 + 0.15 * throwCharge elapsed
                                                                in
                                                                [ WebGL.entityWith
                                                                    [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                                                    vertexShader
                                                                    fragmentShader
                                                                    aimingReticle
                                                                    { ucolor = Vec3.vec3 1 1 1
                                                                    , view = viewMatrix
                                                                    , model =
                                                                        point2ToMatrix targetPosition
                                                                            |> Mat4.scale3 reticleScale reticleScale reticleScale
                                                                    }
                                                                ]

                                                            Nothing ->
                                                                []

                                                    else
                                                        []

                                                Nothing ->
                                                    []

                                        Nothing ->
                                            []
                                   )
                                ++ (case SeqDict.get model.userId state.players of
                                        Just player ->
                                            case player.targetPosition of
                                                Just targetPos ->
                                                    [ WebGL.entityWith
                                                        [ WebGL.Settings.cullFace WebGL.Settings.back ]
                                                        vertexShader
                                                        fragmentShader
                                                        moveArrow
                                                        { ucolor = Vec3.vec3 1 1 1
                                                        , view = viewMatrix
                                                        , model =
                                                            point2ToMatrix targetPos
                                                                |> Mat4.scale3 0.3 0.3 0.3
                                                        }
                                                    ]

                                                Nothing ->
                                                    []

                                        _ ->
                                            []
                                   )

                        Err _ ->
                            []

                Err _ ->
                    []

        _ ->
            []


handPosition : Bool -> Id FrameId -> Player -> Point3d Meters WorldCoordinate
handPosition leftHand frameId player =
    let
        speed : Float
        speed =
            Vector2d.length player.velocity |> Quantity.unwrap

        swingPhase : Float
        swingPhase =
            if leftHand then
                0

            else
                pi

        timeElapsed : Duration
        timeElapsed =
            frameTimeElapsed (Id.fromInt 0) frameId

        isMoving : Bool
        isMoving =
            speed > 0.01

        swingAmount : Length
        swingAmount =
            if isMoving then
                sin (Duration.inSeconds timeElapsed * 10 + swingPhase) * 0.15 |> Length.meters

            else
                Quantity.zero

        -- Check if right hand is charging a throw
        chargeOffset : Length
        chargeOffset =
            if not leftHand then
                case player.clickStart of
                    Just clickStart ->
                        let
                            elapsed =
                                frameTimeElapsed clickStart.time frameId
                        in
                        if elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay then
                            -- Move backwards based on charge amount
                            Length.meters (-0.3 * throwCharge elapsed)

                        else
                            Quantity.zero

                    Nothing ->
                        Quantity.zero

            else
                Quantity.zero

        upOffset : Length
        upOffset =
            if leftHand then
                Length.meters 0.3

            else
                case player.clickStart of
                    Just clickStart ->
                        let
                            elapsed =
                                frameTimeElapsed clickStart.time frameId
                        in
                        if elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay then
                            0.3 * (Length.inMeters snowballStartHeight - 0.3) * throwCharge elapsed |> Length.meters

                        else
                            Length.meters 0.3

                    Nothing ->
                        Length.meters 0.3
    in
    Point2d.translateIn
        (if leftHand then
            Direction2d.rotateCounterclockwise player.rotation

         else
            Direction2d.rotateClockwise player.rotation
        )
        (Length.meters 0.5)
        player.position
        |> Point2d.translateIn player.rotation (Quantity.plus swingAmount chargeOffset)
        |> point2To3 upOffset


drawHand : Bool -> Id FrameId -> Player -> Mat4 -> Mat4 -> WebGL.Entity
drawHand leftHand frameId player viewMatrix modelMatrix =
    WebGL.entityWith
        [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
        vertexShader
        fragmentShader
        playerHand
        { ucolor =
            case player.team of
                BlueTeam ->
                    Vec3.vec3 0 0 0.8

                RedTeam ->
                    Vec3.vec3 0.8 0 0
        , view = viewMatrix
        , model = point3ToMatrix (handPosition leftHand frameId player) |> matMul modelMatrix
        }


matMul : Mat4 -> Mat4 -> Mat4
matMul a b =
    Mat4.mul b a


drawPlayer : Id FrameId -> Id UserId -> MatchActiveLocal_ -> Mat4 -> Player -> List WebGL.Entity
drawPlayer frameId userId matchData viewMatrix player =
    case SeqDict.get userId matchData.userIds of
        Just { skinTone } ->
            let
                rotation : Float
                rotation =
                    Direction2d.toAngle player.rotation |> Angle.inRadians

                playerRadius_ : Float
                playerRadius_ =
                    Length.inMeters playerRadius

                modelMatrix : Mat4
                modelMatrix =
                    Mat4.makeScale3 playerRadius_ playerRadius_ playerRadius_
                        |> Mat4.rotate -0.4 (Vec3.vec3 1 0 0)
                        |> Mat4.rotate rotation (Vec3.vec3 0 0 1)
                        |> (\mat ->
                                case player.isDead of
                                    Just death ->
                                        let
                                            framesSinceDeath : Duration
                                            framesSinceDeath =
                                                frameTimeElapsed death.time frameId

                                            fallProgress : Float
                                            fallProgress =
                                                Quantity.ratio framesSinceDeath (Duration.seconds 0.5) |> min 1

                                            ( dx, dy ) =
                                                Direction2d.rotateClockwise death.fallDirection |> Direction2d.components
                                        in
                                        Mat4.rotate (fallProgress * (pi / 2)) (Vec3.vec3 dx dy 0) mat

                                    Nothing ->
                                        mat
                           )

                ( px, py ) =
                    Point2d.toTuple Length.inMeters player.position
            in
            [ WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                playerShadowMesh
                { ucolor = Vec3.vec3 1 1 1
                , view = viewMatrix
                , model =
                    Mat4.makeTranslate3 px py 0
                        |> Mat4.scale3 playerRadius_ playerRadius_ playerRadius_
                }
            , WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                playerHead
                { ucolor = SkinTone.toVec3 skinTone
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            , WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                playerHat
                { ucolor =
                    case player.team of
                        BlueTeam ->
                            Vec3.vec3 0 0 1

                        RedTeam ->
                            Vec3.vec3 1 0 0
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            , WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                playerHatTuft
                { ucolor =
                    case player.team of
                        BlueTeam ->
                            Vec3.vec3 0.8 0.8 1

                        RedTeam ->
                            Vec3.vec3 1 0.8 0.8
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            , drawHand True frameId player viewMatrix modelMatrix
            , drawHand False frameId player viewMatrix modelMatrix
            , WebGL.entityWith
                [ WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                (case player.isDead of
                    Just _ ->
                        deadPlayerEyes

                    Nothing ->
                        playerEyes
                )
                { ucolor = Vec3.vec3 1 1 1
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            , WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                playerBody
                { ucolor =
                    case player.team of
                        BlueTeam ->
                            Vec3.vec3 0 0 1

                        RedTeam ->
                            Vec3.vec3 1 0 0
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            , WebGL.entityWith
                [ WebGL.Settings.cullFace WebGL.Settings.back, WebGL.Settings.DepthTest.default ]
                vertexShader
                fragmentShader
                playerFeet
                { ucolor =
                    case player.team of
                        BlueTeam ->
                            Vec3.vec3 0 0 0.8

                        RedTeam ->
                            Vec3.vec3 0.8 0 0
                , view = viewMatrix
                , model = point2ToMatrix player.position |> matMul modelMatrix
                }
            ]
                ++ (case player.lastEmote of
                        Just lastEmote ->
                            let
                                timeElapsed : Duration
                                timeElapsed =
                                    Quantity.multiplyBy
                                        (Id.toInt frameId - Id.toInt lastEmote.time |> toFloat)
                                        Match.frameDuration

                                emojiSize : Float
                                emojiSize =
                                    toFromAndBack
                                        (Duration.milliseconds 200)
                                        (Duration.seconds 2)
                                        (Duration.milliseconds 200)
                                        timeElapsed
                                        Ease.outBack
                                        Ease.inBack
                                        0
                                        0.002
                            in
                            drawShape
                                emojiSize
                                (Point2d.translateBy (Vector2d.meters 0.4 0.3) player.position)
                                viewMatrix
                                (case lastEmote.emote of
                                    SurpriseEmote ->
                                        Shape.surprise

                                    ImpEmote ->
                                        Shape.imp
                                )

                        Nothing ->
                            []
                   )

        Nothing ->
            []


drawShape : Float -> Point2d units coordinates -> Mat4 -> RenderableShape -> List WebGL.Entity
drawShape scale position viewMatrix shape =
    List.concatMap
        (\layer ->
            FontRender.drawLayer
                layer.color
                layer.mesh
                (point2ToMatrix position |> Mat4.scale3 scale scale 0.01)
                viewMatrix
        )
        shape.layers


toFrom : Duration -> Duration -> Easing -> Float -> Float -> Float
toFrom duration timeElapsed easingFunction startValue endValue =
    let
        t =
            Quantity.ratio timeElapsed duration |> clamp 0 1
    in
    easingFunction t * (endValue - startValue) + startValue


toFromAndBack : Duration -> Duration -> Duration -> Duration -> Easing -> Easing -> Float -> Float -> Float
toFromAndBack startDuration holdDuration endDuration timeElapsed easingIn easingOut startValue endValue =
    if timeElapsed |> Quantity.lessThan startDuration then
        toFrom startDuration timeElapsed easingIn startValue endValue

    else if timeElapsed |> Quantity.lessThan (Quantity.plus startDuration holdDuration) then
        endValue

    else
        toFrom
            endDuration
            (timeElapsed |> Quantity.minus (Quantity.plus startDuration holdDuration))
            easingOut
            endValue
            startValue


wall : Polygon2d Meters WorldCoordinate
wall =
    Polygon2d.withHoles
        []
        [ Point2d.meters -14 -11
        , Point2d.meters 14 -11
        , Point2d.meters 14 11
        , Point2d.meters -14 11
        ]


playerStart : Point2d Meters WorldCoordinate
playerStart =
    Point2d.fromMeters { x = 0, y = 0 }


wallSegments : List (LineSegment2d Meters WorldCoordinate)
wallSegments =
    Geometry.pointsToLineSegments (Polygon2d.outerLoop wall)
        ++ List.concatMap Geometry.pointsToLineSegments (Polygon2d.innerLoops wall)


gridSize : Length
gridSize =
    Quantity.multiplyBy 2 playerRadius


pointToGrid : Point2d Meters coordinates -> { x : Int, y : Int }
pointToGrid point =
    let
        { x, y } =
            Point2d.scaleAbout Point2d.origin (Quantity.ratio Length.meter gridSize) point |> Point2d.toMeters
    in
    { x = floor x, y = floor y }


wallLookUp : Dict ( Int, Int ) (SeqSet (LineSegment2d Meters WorldCoordinate))
wallLookUp =
    List.foldl
        (\segment dict ->
            let
                ( start, end ) =
                    LineSegment2d.endpoints segment

                addPoint x y =
                    Dict.update ( x, y ) (Maybe.withDefault SeqSet.empty >> SeqSet.insert segment >> Just)
            in
            RasterShapes.line (pointToGrid start) (pointToGrid end)
                |> List.foldl
                    (\{ x, y } dict2 ->
                        dict2
                            |> addPoint (x - 1) (y - 1)
                            |> addPoint x (y - 1)
                            |> addPoint (x + 1) (y - 1)
                            |> addPoint (x - 1) y
                            |> addPoint x y
                            |> addPoint (x + 1) y
                            |> addPoint (x - 1) (y + 1)
                            |> addPoint x (y + 1)
                            |> addPoint (x + 1) (y + 1)
                    )
                    dict
        )
        Dict.empty
        wallSegments


getCollisionCandidates : Point2d Meters coordinates -> SeqSet (LineSegment2d Meters WorldCoordinate)
getCollisionCandidates point =
    Dict.get (pointToGrid point |> (\{ x, y } -> ( x, y ))) wallLookUp |> Maybe.withDefault SeqSet.empty


lineSegmentMesh : Vec3 -> List (LineSegment2d Meters WorldCoordinate) -> Mesh Vertex
lineSegmentMesh color lines =
    List.concatMap (lineMesh (Length.meters 0.1) color) lines |> WebGL.triangles


lineMesh : Quantity Float Meters -> Vec3 -> LineSegment2d Meters WorldCoordinate -> List ( Vertex, Vertex, Vertex )
lineMesh thickness color line =
    let
        ( p0, p1 ) =
            LineSegment2d.endpoints line

        perpendicular : Vector2d units coordinates
        perpendicular =
            Vector2d.from p0 p1
                |> Vector2d.perpendicularTo
                |> Vector2d.normalize
                |> Vector2d.scaleBy (Length.inMeters thickness)
                |> Vector2d.unwrap
                |> Vector2d.unsafe
    in
    [ ( Point2d.translateBy perpendicular p0
      , Point2d.translateBy (Vector2d.reverse perpendicular) p0
      , Point2d.translateBy (Vector2d.reverse perpendicular) p1
      )
    , ( Point2d.translateBy (Vector2d.reverse perpendicular) p1
      , Point2d.translateBy perpendicular p1
      , Point2d.translateBy perpendicular p0
      )
    ]
        |> List.map
            (\( a, b, c ) ->
                ( { position = pointToVec a, color = color }
                , { position = pointToVec b, color = color }
                , { position = pointToVec c, color = color }
                )
            )


pointToVec : Point2d units coordinate -> Vec3
pointToVec point2d =
    let
        { x, y } =
            Point2d.unwrap point2d
    in
    Vec3.vec3 x y 0


clickMoveMaxDelay : Duration
clickMoveMaxDelay =
    Duration.seconds 0.2


chargeMaxDelay : Duration
chargeMaxDelay =
    Duration.seconds 1


clickTotalDelay : Duration
clickTotalDelay =
    Quantity.plus chargeMaxDelay clickMoveMaxDelay


snowballPlayerCollision : Snowball -> Id UserId -> Player -> Bool
snowballPlayerCollision snowball userId player =
    let
        { x, y, z } =
            Point3d.toMeters snowball.position
    in
    (userId /= snowball.thrownBy)
        && (Length.meters z |> Quantity.lessThan playerHeight)
        && (player.isDead == Nothing)
        && (Point2d.distanceFrom (Point2d.meters x y) player.position
                |> Quantity.lessThan (Quantity.plus playerRadius snowballRadius)
           )


playerHeight : Length
playerHeight =
    Length.meters 1.5


updatePlayer : SeqDict (Id UserId) Input -> Id FrameId -> Id UserId -> Player -> MatchState -> MatchState
updatePlayer inputs2 frameId userId player model =
    let
        input : Input
        input =
            if Id.toInt userId < 0 then
                getBotInput frameId model userId player

            else
                SeqDict.get userId inputs2 |> Maybe.withDefault noInput

        ( hitBySnowball, snowballs ) =
            List.foldl
                (\snowball ( hitSnowball, snowballs2 ) ->
                    if hitSnowball == Nothing && snowballPlayerCollision snowball userId player then
                        ( Just snowball, snowballs2 )

                    else
                        ( hitSnowball, snowball :: snowballs2 )
                )
                ( Nothing, [] )
                model.snowballs
                |> Tuple.mapSecond List.reverse

        ( lastStep, footsteps, mergedFootsteps ) =
            if
                Point2d.distanceFrom player.lastStep.position player.position
                    |> Quantity.greaterThan (Length.meters 0.6)
            then
                let
                    newFootstep : List ( Vertex, Vertex, Vertex )
                    newFootstep =
                        footstepMesh player.position player.rotation player.lastStep.stepCount

                    merge =
                        List.length model.footsteps > 20
                in
                ( { position = player.position
                  , time = frameId
                  , stepCount = player.lastStep.stepCount + 1
                  }
                , if merge then
                    []

                  else
                    { position = player.position
                    , rotation = player.rotation
                    , stepCount = player.lastStep.stepCount
                    , mesh = WebGL.triangles newFootstep
                    }
                        :: model.footsteps
                , if merge then
                    WebGL.triangles
                        (newFootstep
                            ++ List.concatMap
                                (\footstep ->
                                    footstepMesh footstep.position footstep.rotation footstep.stepCount
                                )
                                model.footsteps
                        )
                        :: model.mergedFootsteps
                        |> List.take 100

                  else
                    model.mergedFootsteps
                )

            else
                ( player.lastStep, model.footsteps, model.mergedFootsteps )

        players : SeqDict (Id UserId) Player
        players =
            SeqDict.insert
                userId
                { player
                    | targetPosition =
                        case player.isDead of
                            Just _ ->
                                Nothing

                            Nothing ->
                                case ( player.clickStart, input.action ) of
                                    ( Just clickStart, ClickRelease point ) ->
                                        if
                                            frameTimeElapsed clickStart.time frameId
                                                |> Quantity.lessThan clickMoveMaxDelay
                                        then
                                            Just point

                                        else
                                            player.targetPosition

                                    ( Just clickStart, _ ) ->
                                        if
                                            frameTimeElapsed clickStart.time frameId
                                                |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay
                                        then
                                            Nothing

                                        else
                                            player.targetPosition

                                    _ ->
                                        player.targetPosition
                    , rotation =
                        case player.targetPosition of
                            Just targetPosition ->
                                case Direction2d.from player.position targetPosition of
                                    Just direction ->
                                        let
                                            angleDifference : Angle
                                            angleDifference =
                                                Direction2d.angleFrom player.rotation direction
                                        in
                                        if Quantity.abs angleDifference |> Quantity.lessThan (Angle.degrees 5) then
                                            direction

                                        else
                                            Direction2d.rotateBy
                                                (Angle.degrees (5 * Quantity.sign angleDifference))
                                                player.rotation

                                    Nothing ->
                                        player.rotation

                            Nothing ->
                                case player.clickStart of
                                    Just clickStart ->
                                        case Direction2d.from player.position clickStart.position of
                                            Just direction ->
                                                let
                                                    angleDifference : Angle
                                                    angleDifference =
                                                        Direction2d.angleFrom player.rotation direction
                                                in
                                                if Quantity.abs angleDifference |> Quantity.lessThan (Angle.degrees 5) then
                                                    direction

                                                else
                                                    Direction2d.rotateBy
                                                        (Angle.degrees (5 * Quantity.sign angleDifference))
                                                        player.rotation

                                            Nothing ->
                                                player.rotation

                                    Nothing ->
                                        player.rotation
                    , lastEmote =
                        case input.emote of
                            Just emote ->
                                Just { time = frameId, emote = emote }

                            Nothing ->
                                player.lastEmote
                    , clickStart =
                        case player.isDead of
                            Just _ ->
                                Nothing

                            Nothing ->
                                case input.action of
                                    ClickStart point ->
                                        Just { position = point, time = frameId }

                                    ClickRelease _ ->
                                        Nothing

                                    NoAction ->
                                        case player.clickStart of
                                            Just clickStart ->
                                                if
                                                    frameTimeElapsed clickStart.time frameId
                                                        |> Quantity.greaterThanOrEqualTo clickTotalDelay
                                                then
                                                    Nothing

                                                else
                                                    player.clickStart

                                            Nothing ->
                                                player.clickStart
                    , isDead =
                        case hitBySnowball of
                            Just snowball ->
                                case SeqDict.get snowball.thrownBy model.players of
                                    Just thrower ->
                                        if thrower.team == player.team then
                                            player.isDead

                                        else
                                            { time = frameId
                                            , fallDirection =
                                                vector3To2 snowball.velocity
                                                    |> Vector2d.direction
                                                    |> Maybe.withDefault Direction2d.x
                                            }
                                                |> Just

                                    Nothing ->
                                        player.isDead

                            Nothing ->
                                player.isDead
                    , lastStep = lastStep
                }
                model.players
    in
    { model
        | players = players
        , snowballs =
            case ( player.clickStart, input.action ) of
                ( Just clickStart, ClickRelease point ) ->
                    let
                        elapsed : Duration
                        elapsed =
                            frameTimeElapsed clickStart.time frameId
                    in
                    if
                        (elapsed |> Quantity.greaterThanOrEqualTo clickMoveMaxDelay)
                            && (elapsed |> Quantity.lessThan clickTotalDelay)
                    then
                        let
                            direction : Direction2d WorldCoordinate
                            direction =
                                Direction2d.from player.position clickStart.position
                                    |> Maybe.withDefault Direction2d.x

                            { x, y } =
                                Point2d.toMeters player.position

                            distance : Length
                            distance =
                                throwDistance elapsed

                            velocity : Vector3d MetersPerSecond WorldCoordinate
                            velocity =
                                throwVelocity direction distance

                            apexFrame : Maybe (Id FrameId)
                            apexFrame =
                                if distance |> Quantity.lessThan (Quantity.multiplyBy 0.5 maxThrowDistance) then
                                    Nothing

                                else
                                    let
                                        vZ =
                                            Vector3d.zComponent velocity |> Speed.inMetersPerSecond

                                        g =
                                            Acceleration.inMetersPerSecondSquared gravity |> abs

                                        timeToApex =
                                            vZ / g

                                        framesToApex =
                                            timeToApex / Duration.inSeconds Match.frameDuration |> round
                                    in
                                    Just (Id.fromInt (Id.toInt frameId + framesToApex))
                        in
                        { thrownBy = userId
                        , thrownAt = frameId
                        , velocity = velocity
                        , position = Point3d.meters x y (Length.inMeters snowballStartHeight)
                        , apexFrame = apexFrame
                        , isOvercharge = False
                        }
                            :: snowballs

                    else
                        snowballs

                ( Just clickStart, _ ) ->
                    if frameTimeElapsed clickStart.time frameId |> Quantity.greaterThanOrEqualTo clickTotalDelay then
                        -- Overcharge: very short throw that becomes pushable when it lands
                        let
                            direction : Direction2d WorldCoordinate
                            direction =
                                Direction2d.from player.position clickStart.position
                                    |> Maybe.withDefault Direction2d.x

                            { x, y } =
                                Point2d.toMeters player.position

                            velocity : Vector3d MetersPerSecond WorldCoordinate
                            velocity =
                                throwVelocity direction overchargeThrowDistance
                        in
                        { thrownBy = userId
                        , thrownAt = frameId
                        , velocity = velocity
                        , position = Point3d.meters x y (Length.inMeters snowballStartHeight)
                        , apexFrame = Nothing
                        , isOvercharge = True
                        }
                            :: snowballs

                    else
                        snowballs

                _ ->
                    snowballs
        , particles =
            case hitBySnowball of
                Just snowball ->
                    snowballParticles frameId snowball.thrownAt snowball.position ++ model.particles

                Nothing ->
                    model.particles
        , footsteps = footsteps
        , mergedFootsteps = mergedFootsteps
        , roundEndTime =
            case hitBySnowball of
                Just _ ->
                    checkWinningTeam frameId players model.roundEndTime

                Nothing ->
                    model.roundEndTime
        , snowballImpacts =
            case hitBySnowball of
                Just _ ->
                    frameId :: model.snowballImpacts

                Nothing ->
                    model.snowballImpacts
    }


gameUpdate : Id FrameId -> List TimelineEvent -> MatchState -> MatchState
gameUpdate frameId inputs model =
    let
        inputs2 : SeqDict (Id UserId) Input
        inputs2 =
            List.map (\input -> ( input.userId, input.input )) inputs |> SeqDict.fromList

        model2 : MatchState
        model2 =
            SeqDict.foldl (updatePlayer inputs2 frameId) model model.players

        updatedVelocities_ : SeqDict (Id UserId) Player
        updatedVelocities_ =
            updateVelocities frameId model2.players

        ( survivingSnowballs, newParticles, newPushableSnowballs ) =
            List.foldl
                (\snowball ( snowballs2, particles2, pushable2 ) ->
                    let
                        position : Point3d Meters WorldCoordinate
                        position =
                            Point3d.translateBy (Vector3d.for Match.frameDuration snowball.velocity) snowball.position
                    in
                    if Point3d.zCoordinate position |> Quantity.greaterThanZero then
                        ( { position = position
                          , velocity = Vector3d.plus (Vector3d.for Match.frameDuration gravityVector) snowball.velocity
                          , thrownBy = snowball.thrownBy
                          , thrownAt = snowball.thrownAt
                          , apexFrame = snowball.apexFrame
                          , isOvercharge = snowball.isOvercharge
                          }
                            :: snowballs2
                        , particles2
                        , pushable2
                        )

                    else if snowball.isOvercharge then
                        -- Overcharge snowball lands and becomes pushable
                        let
                            { x, y } =
                                Point3d.toMeters position
                        in
                        ( snowballs2
                        , particles2
                        , { position = Point2d.meters x y, velocity = Vector2d.zero, radius = pushableSnowballStartRadius } :: pushable2
                        )

                    else
                        ( snowballs2
                        , snowballParticles frameId snowball.thrownAt position ++ particles2
                        , pushable2
                        )
                )
                ( [], [], [] )
                model2.snowballs

        particles : List Particle
        particles =
            List.filter
                (\particle -> frameTimeElapsed particle.spawnedAt frameId |> Quantity.lessThan particle.lifetime)
                model2.particles
                ++ newParticles

        snowballImpacts : List (Id FrameId)
        snowballImpacts =
            List.filter
                (\impactTime -> frameTimeElapsed impactTime frameId |> Quantity.lessThan Duration.second)
                model2.snowballImpacts
                ++ List.map
                    (\_ -> frameId)
                    (List.range 1 (List.length model2.snowballs - List.length survivingSnowballs))
    in
    case model.roundEndTime of
        Just roundEnd ->
            if frameTimeElapsed roundEnd.time frameId |> Quantity.lessThan roundResetDuration then
                let
                    finalPlayers =
                        SeqDict.map
                            (\id player ->
                                SeqDict.remove id updatedVelocities_
                                    |> SeqDict.values
                                    |> List.foldl (\a b -> handleCollision frameId b a |> Tuple.first) player
                            )
                            updatedVelocities_
                in
                { players = finalPlayers
                , snowballs = List.reverse survivingSnowballs
                , pushableSnowballs =
                    updatePushableSnowballs finalPlayers (model2.pushableSnowballs ++ newPushableSnowballs)
                , particles = particles
                , footsteps = model2.footsteps
                , mergedFootsteps = model2.mergedFootsteps
                , score = model2.score
                , roundEndTime = model2.roundEndTime
                , snowballImpacts = snowballImpacts
                }

            else
                { players = initPlayerPosition model2.players
                , snowballs = []
                , pushableSnowballs = model2.pushableSnowballs
                , particles = []
                , footsteps = model2.footsteps
                , mergedFootsteps = model2.mergedFootsteps
                , score =
                    case roundEnd.winner of
                        BothWon ->
                            { redTeam = model2.score.redTeam + 1, blueTeam = model2.score.blueTeam + 1 }

                        RedWon ->
                            { redTeam = model2.score.redTeam + 1, blueTeam = model2.score.blueTeam }

                        BlueWon ->
                            { redTeam = model2.score.redTeam, blueTeam = model2.score.blueTeam + 1 }
                , roundEndTime = Nothing
                , snowballImpacts = []
                }

        Nothing ->
            let
                finalPlayers =
                    SeqDict.map
                        (\id player ->
                            SeqDict.remove id updatedVelocities_
                                |> SeqDict.values
                                |> List.foldl (\a b -> handleCollision frameId b a |> Tuple.first) player
                        )
                        updatedVelocities_
            in
            { players = finalPlayers
            , snowballs = List.reverse survivingSnowballs
            , pushableSnowballs =
                updatePushableSnowballs finalPlayers (model2.pushableSnowballs ++ newPushableSnowballs)
            , particles = particles
            , footsteps = model2.footsteps
            , mergedFootsteps = model2.mergedFootsteps
            , score = model2.score
            , roundEndTime = model2.roundEndTime
            , snowballImpacts = snowballImpacts
            }


roundResetDuration : Duration
roundResetDuration =
    Duration.seconds 5


checkWinningTeam :
    Id FrameId
    -> SeqDict (Id UserId) Player
    -> Maybe { winner : Winner, time : Id FrameId }
    -> Maybe { winner : Winner, time : Id FrameId }
checkWinningTeam frameId players existingWinner =
    let
        alivePlayers =
            SeqDict.values players |> List.filter (\player -> player.isDead == Nothing)

        maybeWinner =
            case
                ( List.any (\player -> player.team == RedTeam) alivePlayers
                , List.any (\player -> player.team == BlueTeam) alivePlayers
                )
            of
                ( True, True ) ->
                    Nothing

                ( True, False ) ->
                    Just RedWon

                ( False, True ) ->
                    Just BlueWon

                ( False, False ) ->
                    Just BothWon
    in
    case ( maybeWinner, existingWinner ) of
        ( Just winner, Just existingWinner2 ) ->
            if winner == existingWinner2.winner then
                existingWinner

            else
                Just { winner = winner, time = frameId }

        ( Just winner, Nothing ) ->
            Just { winner = winner, time = frameId }

        ( Nothing, _ ) ->
            existingWinner


vector3To2 : Vector3d u c -> Vector2d u c
vector3To2 v =
    let
        { x, y } =
            Vector3d.unwrap v
    in
    Vector2d.unsafe { x = x, y = y }


vector2To3 : Quantity Float units -> Vector2d units coordinates -> Vector3d units coordinates
vector2To3 z v =
    Vector3d.xyz (Vector2d.xComponent v) (Vector2d.yComponent v) z


point2To3 : Quantity Float u -> Point2d u c -> Point3d u c
point2To3 z p =
    Point3d.xyz (Point2d.xCoordinate p) (Point2d.yCoordinate p) z


snowballParticles : Id FrameId -> Id FrameId -> Point3d Meters WorldCoordinate -> List Particle
snowballParticles frameId thrownAt impactPosition =
    let
        { x, y, z } =
            Point3d.toMeters impactPosition

        count : number
        count =
            8

        particleGenerator : Int -> Random.Generator Particle
        particleGenerator index =
            Random.map3
                (\size lifetime speed ->
                    { position = impactPosition
                    , velocity =
                        Vector2d.withLength
                            (Speed.metersPerSecond speed)
                            ((toFloat index / count) |> Angle.turns |> Direction2d.fromAngle)
                    , size = Length.meters size
                    , spawnedAt = frameId
                    , lifetime = Duration.seconds lifetime
                    }
                )
                (Random.float 0.1 0.3)
                (Random.float 0.1 0.2)
                (Random.float 2 4)
    in
    Random.step
        (List.range 0 (count - 1) |> List.map particleGenerator |> Random.Extra.sequence)
        (Random.initialSeed (Id.toInt frameId * 1000 + Id.toInt thrownAt))
        |> Tuple.first


sphere : Vec3 -> Vec3 -> Vec3 -> TriangularMesh Vertex
sphere position scaleBy color =
    let
        p =
            Vec3.toRecord position

        s =
            Vec3.toRecord scaleBy

        uDetail =
            18

        vDetail =
            10
    in
    TriangularMesh.indexedBall
        uDetail
        vDetail
        (\u v ->
            let
                longitude =
                    2 * pi * toFloat u / toFloat uDetail

                latitude =
                    pi * toFloat v / toFloat vDetail
            in
            { position =
                Vec3.vec3
                    (sin longitude * sin latitude * s.x + p.x)
                    (cos longitude * sin latitude * s.y + p.y)
                    (cos latitude * s.z + p.z)
            , color = color
            }
        )


sphereMesh : Vec3 -> Vec3 -> Vec3 -> Mesh Vertex
sphereMesh position scaleBy color =
    sphere position scaleBy color |> TriangularMesh.faceVertices |> WebGL.triangles


playerHead : Mesh Vertex
playerHead =
    sphereMesh (Vec3.vec3 0 0 1.5) (Vec3.vec3 0.9 0.9 0.9) (Vec3.vec3 1 1 1)


playerHat : Mesh Vertex
playerHat =
    sphereMesh (Vec3.vec3 -0.2 0 1.9) (Vec3.vec3 0.8 0.8 0.8) (Vec3.vec3 1 1 1)


playerHatTuft : Mesh Vertex
playerHatTuft =
    sphereMesh (Vec3.vec3 -0.6 0 2.7) (Vec3.vec3 0.3 0.3 0.3) (Vec3.vec3 1 1 1)


playerBody : Mesh Vertex
playerBody =
    sphereMesh (Vec3.vec3 0 0 0.7) (Vec3.vec3 1 1 1.2) (Vec3.vec3 1 1 1)


playerHand : Mesh Vertex
playerHand =
    sphereMesh (Vec3.vec3 0 0 0.7) (Vec3.vec3 0.3 0.3 0.3) (Vec3.vec3 1 1 1)


playerFeet : Mesh Vertex
playerFeet =
    let
        leftFoot =
            sphere (Vec3.vec3 0 0.35 -0.4) (Vec3.vec3 0.35 0.35 0.25) (Vec3.vec3 1 1 1) |> TriangularMesh.faceVertices

        rightFoot =
            sphere (Vec3.vec3 0 -0.35 -0.4) (Vec3.vec3 0.35 0.35 0.25) (Vec3.vec3 1 1 1) |> TriangularMesh.faceVertices
    in
    leftFoot ++ rightFoot |> WebGL.triangles


playerEyes : Mesh Vertex
playerEyes =
    let
        leftEye =
            sphere (Vec3.vec3 0.8 0.4 1.6) (Vec3.vec3 0.12 0.12 0.12) (Vec3.vec3 0 0 0) |> TriangularMesh.faceVertices

        rightEye =
            sphere (Vec3.vec3 0.8 -0.4 1.6) (Vec3.vec3 0.12 0.12 0.12) (Vec3.vec3 0 0 0) |> TriangularMesh.faceVertices
    in
    leftEye ++ rightEye |> WebGL.triangles


deadPlayerEyes : Mesh Vertex
deadPlayerEyes =
    let
        size : Float
        size =
            0.2

        z : Float
        z =
            1.6

        xLine : Float -> List ( Vertex, Vertex, Vertex )
        xLine y =
            let
                thickness =
                    0.1

                color =
                    Vec3.vec3 0 0 0

                line1 =
                    [ ( { position = Vec3.vec3 0.9 (y - size - thickness) (z - size), color = color }
                      , { position = Vec3.vec3 0.9 (y - size + thickness) (z - size), color = color }
                      , { position = Vec3.vec3 0.9 (y + size + thickness) (z + size), color = color }
                      )
                    , ( { position = Vec3.vec3 0.9 (y + size + thickness) (z + size), color = color }
                      , { position = Vec3.vec3 0.9 (y + size - thickness) (z + size), color = color }
                      , { position = Vec3.vec3 0.9 (y - size - thickness) (z - size), color = color }
                      )
                    ]

                line2 =
                    [ ( { position = Vec3.vec3 0.9 (y - size - thickness) (z + size), color = color }
                      , { position = Vec3.vec3 0.9 (y - size + thickness) (z + size), color = color }
                      , { position = Vec3.vec3 0.9 (y + size + thickness) (z - size), color = color }
                      )
                    , ( { position = Vec3.vec3 0.9 (y + size + thickness) (z - size), color = color }
                      , { position = Vec3.vec3 0.9 (y + size - thickness) (z - size), color = color }
                      , { position = Vec3.vec3 0.9 (y - size - thickness) (z + size), color = color }
                      )
                    ]
            in
            line1 ++ line2
    in
    (xLine 0.4 ++ xLine -0.4) |> WebGL.triangles


getBotInput : Id FrameId -> MatchState -> Id UserId -> Player -> Input
getBotInput frameId _ userId player =
    case player.isDead of
        Just _ ->
            { action = NoAction, emote = Nothing }

        Nothing ->
            let
                seed =
                    Random.initialSeed (Id.toInt frameId + Id.toInt userId * 1000)

                shouldPickNewTarget =
                    player.targetPosition == Nothing || modBy 60 (Id.toInt frameId + Id.toInt userId * 7) == 0

                ( randomX, seed2 ) =
                    Random.step (Random.float -2 8) seed

                ( randomY, _ ) =
                    Random.step (Random.float -2 8) seed2

                newTargetPosition =
                    Point2d.meters randomX randomY
            in
            case player.clickStart of
                Nothing ->
                    if shouldPickNewTarget then
                        { action = ClickStart newTargetPosition, emote = Nothing }

                    else
                        noInput

                Just _ ->
                    { action = ClickRelease newTargetPosition, emote = Nothing }


gravity : Acceleration
gravity =
    Acceleration.metersPerSecondSquared -4


gravityVector : Vector3d MetersPerSecondSquared WorldCoordinate
gravityVector =
    Vector3d.xyz Quantity.zero Quantity.zero gravity


snowballStartHeight : Length
snowballStartHeight =
    Length.meters 1


maxThrowDistance : Length
maxThrowDistance =
    Length.meters 15


overchargeThrowDistance : Length
overchargeThrowDistance =
    throwDistance Quantity.zero


throwVelocity : Direction2d WorldCoordinate -> Length -> Vector3d MetersPerSecond WorldCoordinate
throwVelocity direction distance =
    let
        -- For projectile motion starting at height h above ground:
        -- The ball lands when y = 0, giving flight time from quadratic formula
        -- Range at 45°: R_45 = v*(v + sqrt(v² + 4gh)) / (2g)
        -- Solving R_45 = R_max gives: v² = g*R_max² / (R_max + h)
        g =
            Acceleration.inMetersPerSecondSquared gravity |> abs

        h =
            Length.inMeters snowballStartHeight

        rMax =
            Length.inMeters maxThrowDistance

        -- Constant throw speed calibrated so 45° gives maxThrowDistance
        vSquared =
            g * rMax * rMax / (rMax + h)

        v =
            sqrt vSquared

        d =
            Length.inMeters distance |> clamp 0.001 rMax

        -- Find launch angle using the trajectory equation:
        -- g*d²*tan²(θ) - 2*v²*d*tan(θ) + (g*d² - 2*v²*h) = 0
        -- Discriminant: D = v⁴ + 2*g*v²*h - g²*d²
        -- tan(θ) = (v² ± sqrt(D)) / (g*d)
        -- Use - solution to get lower/downward angles for short distances
        discriminant =
            vSquared * vSquared + 2 * g * vSquared * h - g * g * d * d

        tanTheta =
            (vSquared - sqrt (max 0 discriminant)) / (g * d)

        launchAngle =
            atan tanTheta

        -- Velocity components
        horizontalSpeed =
            v * cos launchAngle

        verticalSpeed =
            v * sin launchAngle

        -- Get the 2D direction components
        ( dirX, dirY ) =
            Direction2d.components direction
    in
    Vector3d.metersPerSecond
        (dirX * horizontalSpeed)
        (dirY * horizontalSpeed)
        verticalSpeed


throwCharge : Duration -> Float
throwCharge clickStartElapsed =
    let
        t : Float
        t =
            Quantity.ratio (clickStartElapsed |> Quantity.minus clickMoveMaxDelay) chargeMaxDelay |> max 0

        offset =
            0.1
    in
    if t < 0.5 then
        (t * 2) * (1 - offset) + offset

    else
        (2 - t * 2) * (1 - offset) + offset


throwDistance : Duration -> Length
throwDistance clickStartElapsed =
    Quantity.multiplyBy (throwCharge clickStartElapsed) maxThrowDistance


updateVelocities : Id FrameId -> SeqDict (Id UserId) Player -> SeqDict (Id UserId) Player
updateVelocities frameId players =
    let
        elapsed : Duration
        elapsed =
            Quantity.multiplyBy (Id.toInt frameId |> toFloat) Match.frameDuration
    in
    SeqDict.map
        (\_ a ->
            let
                nearestCollision :
                    Maybe
                        { collisionVelocity : Vector2d Meters WorldCoordinate
                        , collisionPosition : Point2d Meters WorldCoordinate
                        }
                nearestCollision =
                    getCollisionCandidates a.position
                        |> SeqSet.toList
                        |> List.filterMap
                            (\line ->
                                let
                                    lineCollision =
                                        Geometry.circleLine playerRadius a.position a.velocity line
                                in
                                case ( lineCollision, LineSegment2d.direction line ) of
                                    ( Just collisionPosition, Just lineDirection ) ->
                                        { collisionPosition = collisionPosition
                                        , collisionVelocity =
                                            Vector2d.mirrorAcross
                                                (Axis2d.withDirection lineDirection (LineSegment2d.startPoint line))
                                                newVelocity
                                        }
                                            |> Just

                                    _ ->
                                        let
                                            point : Point2d Meters WorldCoordinate
                                            point =
                                                LineSegment2d.startPoint line
                                        in
                                        case Geometry.circlePoint playerRadius a.position a.velocity point of
                                            Just collisionPoint ->
                                                case Direction2d.from collisionPoint point of
                                                    Just direction ->
                                                        { collisionPosition = collisionPoint
                                                        , collisionVelocity =
                                                            Vector2d.mirrorAcross
                                                                (Axis2d.withDirection
                                                                    (Direction2d.perpendicularTo direction)
                                                                    collisionPoint
                                                                )
                                                                newVelocity
                                                        }
                                                            |> Just

                                                    Nothing ->
                                                        Nothing

                                            Nothing ->
                                                Nothing
                            )
                        |> Quantity.sortBy (.collisionPosition >> Point2d.distanceFrom a.position)
                        |> List.head

                newVelocity : Vector2d Meters WorldCoordinate
                newVelocity =
                    (case ( elapsed |> Quantity.lessThan countdownDelay, a.targetPosition ) of
                        ( False, Just targetPos ) ->
                            let
                                distance =
                                    Vector2d.from a.position targetPos
                            in
                            distance
                                |> Vector2d.normalize
                                |> Vector2d.scaleBy 0.0015
                                |> Vector2d.unwrap
                                |> Vector2d.unsafe

                        _ ->
                            Vector2d.zero
                    )
                        |> Vector2d.plus a.velocity
                        |> Vector2d.scaleBy 0.95

                newTargetPosition : Maybe (Point2d Meters WorldCoordinate)
                newTargetPosition =
                    case a.targetPosition of
                        Just targetPos ->
                            if Point2d.distanceFrom a.position targetPos |> Quantity.lessThan stopAtDistance then
                                Nothing

                            else
                                a.targetPosition

                        Nothing ->
                            Nothing
            in
            case nearestCollision of
                Just { collisionVelocity, collisionPosition } ->
                    { position = collisionPosition
                    , targetPosition = newTargetPosition
                    , velocity = collisionVelocity
                    , rotation = a.rotation
                    , lastEmote = a.lastEmote
                    , clickStart = a.clickStart
                    , isDead = a.isDead
                    , team = a.team
                    , lastStep = a.lastStep
                    }

                Nothing ->
                    { position = Point2d.translateBy a.velocity a.position
                    , targetPosition = newTargetPosition
                    , velocity = newVelocity
                    , rotation = a.rotation
                    , lastEmote = a.lastEmote
                    , clickStart = a.clickStart
                    , isDead = a.isDead
                    , team = a.team
                    , lastStep = a.lastStep
                    }
        )
        players


stopAtDistance : Length
stopAtDistance =
    Length.meters 0.3


playerRadius : Length
playerRadius =
    Length.meters 0.5


arrow : Vec3 -> Mesh Vertex
arrow color =
    [ { v0 = ( -1, 1 ), v1 = ( 0, 0 ), v2 = ( 1, 1 ) }
    , { v0 = ( -0.5, 1 ), v1 = ( 0.5, 1 ), v2 = ( 0.5, 2 ) }
    , { v0 = ( -0.5, 2 ), v1 = ( -0.5, 1 ), v2 = ( 0.5, 2 ) }
    ]
        |> List.map
            (\{ v0, v1, v2 } ->
                ( { position = Vec3.vec3 (Tuple.first v0) (Tuple.second v0) 0
                  , color = color
                  }
                , { position = Vec3.vec3 (Tuple.first v1) (Tuple.second v1) 0
                  , color = color
                  }
                , { position = Vec3.vec3 (Tuple.first v2) (Tuple.second v2) 0
                  , color = color
                  }
                )
            )
        |> WebGL.triangles


moveArrow : WebGL.Mesh Vertex
moveArrow =
    arrow (Vec3.vec3 1 0.8 0.1)


aimingReticle : WebGL.Mesh Vertex
aimingReticle =
    let
        color =
            Vec3.vec3 0.3 0.7 1

        segments =
            32

        innerRadius =
            0.7

        outerRadius =
            1.0

        lineThickness =
            0.08

        toVertex ( x, y ) =
            { position = Vec3.vec3 x y 0, color = color }

        -- Ring segments
        ringTriangles =
            List.range 0 (segments - 1)
                |> List.concatMap
                    (\i ->
                        let
                            angle1 =
                                2 * pi * toFloat i / toFloat segments

                            angle2 =
                                2 * pi * toFloat (i + 1) / toFloat segments

                            inner1 =
                                ( innerRadius * cos angle1, innerRadius * sin angle1 )

                            outer1 =
                                ( outerRadius * cos angle1, outerRadius * sin angle1 )

                            inner2 =
                                ( innerRadius * cos angle2, innerRadius * sin angle2 )

                            outer2 =
                                ( outerRadius * cos angle2, outerRadius * sin angle2 )
                        in
                        [ ( toVertex inner1, toVertex outer1, toVertex outer2 )
                        , ( toVertex inner1, toVertex outer2, toVertex inner2 )
                        ]
                    )

        -- Horizontal crosshair
        horizontalCrosshair =
            [ ( toVertex ( -outerRadius, -lineThickness )
              , toVertex ( outerRadius, -lineThickness )
              , toVertex ( outerRadius, lineThickness )
              )
            , ( toVertex ( -outerRadius, -lineThickness )
              , toVertex ( outerRadius, lineThickness )
              , toVertex ( -outerRadius, lineThickness )
              )
            ]

        -- Vertical crosshair
        verticalCrosshair =
            [ ( toVertex ( -lineThickness, -outerRadius )
              , toVertex ( lineThickness, -outerRadius )
              , toVertex ( lineThickness, outerRadius )
              )
            , ( toVertex ( -lineThickness, -outerRadius )
              , toVertex ( lineThickness, outerRadius )
              , toVertex ( -lineThickness, outerRadius )
              )
            ]
    in
    (ringTriangles ++ horizontalCrosshair ++ verticalCrosshair)
        |> WebGL.triangles


handleCollision : Id FrameId -> Player -> Player -> ( Player, Player )
handleCollision frameId playerA playerB =
    let
        distance =
            Point2d.distanceFrom playerA.position playerB.position

        minDistance =
            Quantity.multiplyBy 2 playerRadius
    in
    if distance |> Quantity.lessThan minDistance then
        case Direction2d.from playerA.position playerB.position of
            Just direction ->
                let
                    overlap =
                        Quantity.minus distance minDistance

                    halfOverlap =
                        Quantity.multiplyBy 0.5 overlap
                in
                ( { playerA
                    | position = Point2d.translateIn (Direction2d.reverse direction) halfOverlap playerA.position
                  }
                , { playerB
                    | position = Point2d.translateIn direction halfOverlap playerB.position
                  }
                )

            Nothing ->
                -- Players are at the exact same position, push them apart in an arbitrary direction
                let
                    halfOverlap =
                        Quantity.multiplyBy 0.5 minDistance
                in
                ( { playerA
                    | position = Point2d.translateIn Direction2d.negativeX halfOverlap playerA.position
                  }
                , { playerB
                    | position = Point2d.translateIn Direction2d.positiveX halfOverlap playerB.position
                  }
                )

    else
        ( playerA, playerB )


{-| Update all pushable snowballs: move them, apply friction, handle collisions with players and each other
-}
updatePushableSnowballs : SeqDict (Id UserId) Player -> List Match.PushableSnowball -> List Match.PushableSnowball
updatePushableSnowballs players pushableSnowballs =
    let
        players2 =
            SeqDict.values players

        -- First, apply player collisions and update velocities
        afterPlayerCollisions =
            List.map (\snowball -> List.foldl pushableSnowballPlayerCollision snowball players2) pushableSnowballs

        -- Then, handle collisions between pushable snowballs themselves
        afterSnowballCollisions =
            handlePushableSnowballCollisions afterPlayerCollisions

        -- Finally, apply friction, move snowballs, and grow them if moving
        friction =
            0.95

        -- Minimum speed to grow (meters per frame)
        growthSpeedThreshold =
            Length.meters 0.01

        -- Growth rate per frame when moving
        growthRate =
            Length.meters 0.002

        -- Maximum radius the snowball can grow to
        maxRadius =
            Length.meters 0.8
    in
    List.map
        (\snowball ->
            let
                newVelocity =
                    Vector2d.scaleBy friction snowball.velocity

                newPosition =
                    Point2d.translateBy newVelocity snowball.position

                speed =
                    Vector2d.length snowball.velocity

                newRadius =
                    if speed |> Quantity.greaterThan growthSpeedThreshold then
                        Quantity.plus snowball.radius growthRate
                            |> Quantity.min maxRadius

                    else
                        snowball.radius
            in
            { snowball | position = newPosition, velocity = newVelocity, radius = newRadius }
        )
        afterSnowballCollisions


{-| Handle collision between a player and a pushable snowball
-}
pushableSnowballPlayerCollision : Player -> Match.PushableSnowball -> Match.PushableSnowball
pushableSnowballPlayerCollision player snowball =
    let
        distance =
            Point2d.distanceFrom player.position snowball.position

        minDistance =
            Quantity.plus playerRadius snowball.radius
    in
    if distance |> Quantity.lessThan minDistance then
        let
            direction =
                Direction2d.from snowball.position player.position |> Maybe.withDefault Direction2d.x

            -- Push the snowball away from the player
            overlap =
                distance |> Quantity.minus minDistance

            -- Add velocity based on player's movement and push direction
            pushStrength =
                Length.meters -0.01

            pushVelocity =
                Vector2d.withLength pushStrength direction

            -- Also inherit some of player's velocity
            inheritedVelocity =
                Vector2d.scaleBy 0.5 player.velocity

            newVelocity =
                Vector2d.plus inheritedVelocity pushVelocity

            -- Move snowball out of collision
            newPosition =
                Point2d.translateIn direction overlap snowball.position
        in
        { snowball | position = newPosition, velocity = newVelocity }

    else
        snowball


{-| Handle collisions between all pushable snowballs
-}
handlePushableSnowballCollisions : List Match.PushableSnowball -> List Match.PushableSnowball
handlePushableSnowballCollisions snowballs =
    List.map
        (\current ->
            let
                allCollisions : List ( Point2d Meters WorldCoordinate, Vector2d Meters WorldCoordinate )
                allCollisions =
                    List.filterMap
                        (\other ->
                            if other == current then
                                Nothing

                            else
                                let
                                    distance =
                                        Point2d.distanceFrom current.position other.position

                                    minDistance =
                                        Quantity.plus current.radius other.radius
                                in
                                case Geometry.circleCircle current.radius other.radius current.position current.velocity other.position other.velocity of
                                    Just ( newVelA, newVelB ) ->
                                        let
                                            direction =
                                                Direction2d.from current.position other.position |> Maybe.withDefault Direction2d.x

                                            overlap =
                                                Quantity.minus distance minDistance

                                            halfOverlap =
                                                Quantity.multiplyBy 0.5 overlap
                                        in
                                        ( Point2d.translateIn (Direction2d.reverse direction) halfOverlap current.position
                                        , newVelA
                                        )
                                            |> Just

                                    Nothing ->
                                        Nothing
                        )
                        snowballs
            in
            case allCollisions of
                [] ->
                    current

                first :: rest ->
                    { current
                        | position = Point2d.centroidOf Tuple.first first rest
                        , velocity =
                            List.map Tuple.second allCollisions
                                |> Vector2d.sum
                                |> Vector2d.scaleBy (1 / toFloat (List.length allCollisions))
                    }
        )
        snowballs


squareMesh : WebGL.Mesh { position : Vec2 }
squareMesh =
    WebGL.triangleFan
        [ { position = Vec2.vec2 -1 -1 }
        , { position = Vec2.vec2 1 -1 }
        , { position = Vec2.vec2 1 1 }
        , { position = Vec2.vec2 -1 1 }
        ]


circleMesh : Float -> Vec3 -> List ( Vertex, Vertex, Vertex )
circleMesh size color =
    let
        detail =
            64
    in
    List.range 0 (detail - 3)
        |> List.map
            (\index ->
                let
                    t0 =
                        0

                    t1 =
                        pi * 2 * toFloat (index + 1) / detail

                    t2 =
                        pi * 2 * toFloat (index + 2) / detail
                in
                ( { position = Vec3.vec3 (cos t0 * size) (sin t0 * size) 0, color = color }
                , { position = Vec3.vec3 (cos t1 * size) (sin t1 * size) 0, color = color }
                , { position = Vec3.vec3 (cos t2 * size) (sin t2 * size) 0, color = color }
                )
            )


ovalMesh : Point3d Meters WorldCoordinate -> Vec2 -> Float -> Vec3 -> List ( Vertex, Vertex, Vertex )
ovalMesh position scale radiansRotation color =
    let
        detail =
            8

        p =
            Point3d.toMeters position

        s : { x : Float, y : Float }
        s =
            Vec2.toRecord scale

        cosR =
            cos radiansRotation

        sinR =
            sin radiansRotation

        pointAt : Float -> Vec3
        pointAt angle =
            let
                x =
                    cos angle * s.x

                y =
                    sin angle * s.y

                rotatedX =
                    x * cosR - y * sinR + p.x

                rotatedY =
                    x * sinR + y * cosR + p.y
            in
            Vec3.vec3 rotatedX rotatedY p.z
    in
    List.range 0 (detail - 3)
        |> List.map
            (\index ->
                let
                    t0 =
                        0

                    t1 =
                        pi * 2 * toFloat (index + 1) / detail

                    t2 =
                        pi * 2 * toFloat (index + 2) / detail
                in
                ( { position = pointAt t0, color = color }
                , { position = pointAt t1, color = color }
                , { position = pointAt t2, color = color }
                )
            )


footstepMesh : Point2d Meters WorldCoordinate -> Direction2d WorldCoordinate -> Int -> List ( Vertex, Vertex, Vertex )
footstepMesh position direction stepCount =
    let
        topMesh =
            ovalMesh
                (Point2d.translateIn
                    (if modBy 2 stepCount == 0 then
                        Direction2d.rotateClockwise direction

                     else
                        Direction2d.rotateCounterclockwise direction
                    )
                    (Length.meters 0.2)
                    position
                    |> point2To3 (Length.meters -0.01)
                )
                (Vec2.vec2 0.2 0.1)
                (Direction2d.toAngle direction |> Angle.inRadians)
                (Vec3.vec3 1 1 1)

        bottomMesh =
            ovalMesh
                (Point2d.translateIn
                    (if modBy 2 stepCount == 0 then
                        Direction2d.rotateClockwise direction

                     else
                        Direction2d.rotateCounterclockwise direction
                    )
                    (Length.meters 0.2)
                    position
                    |> Point2d.translateBy (Vector2d.meters 0 0.05)
                    |> point2To3 (Length.meters -0.02)
                )
                (Vec2.vec2 0.2 0.1)
                (Direction2d.toAngle direction |> Angle.inRadians)
                (Vec3.vec3 0.8 0.8 0.8)
    in
    topMesh ++ bottomMesh


snowballMesh : WebGL.Mesh Vertex
snowballMesh =
    circleMesh 0.85 (Vec3.vec3 1 1 1)
        ++ circleMesh 1 (Vec3.vec3 0 0 0)
        |> WebGL.triangles


snowballShadowMesh : WebGL.Mesh Vertex
snowballShadowMesh =
    circleMesh 1 (Vec3.vec3 0.2 0.2 0.2)
        |> WebGL.triangles


playerShadowMesh : WebGL.Mesh Vertex
playerShadowMesh =
    circleMesh 1 (Vec3.vec3 0.8 0.8 0.8)
        |> WebGL.triangles


particleMesh : WebGL.Mesh Vertex
particleMesh =
    circleMesh 0.9 (Vec3.vec3 1 1 1)
        |> WebGL.triangles


particleOutlineMesh : WebGL.Mesh Vertex
particleOutlineMesh =
    circleMesh 1 (Vec3.vec3 0 0 0)
        |> WebGL.triangles


snowballRadius : Quantity Float Meters
snowballRadius =
    Length.meters 0.2


pushableSnowballStartRadius : Quantity Float Meters
pushableSnowballStartRadius =
    Length.meters 0.4


pushableSnowballMesh : WebGL.Mesh Vertex
pushableSnowballMesh =
    flatBottomCircleMesh 1 0.1 (Vec3.vec3 1 1 1)
        |> WebGL.triangles


flatBottomCircleMesh : Float -> Float -> Vec3 -> List ( Vertex, Vertex, Vertex )
flatBottomCircleMesh radius flattenAmount color =
    let
        detail =
            64

        -- The y-coordinate threshold below which we flatten
        minY =
            -radius * (1 - flattenAmount)

        adjustY y =
            max minY y
    in
    List.range 0 (detail - 1)
        |> List.map
            (\index ->
                let
                    t1 =
                        pi * 2 * toFloat index / detail

                    t2 =
                        pi * 2 * toFloat (index + 1) / detail

                    y1 =
                        adjustY (sin t1 * radius)

                    y2 =
                        adjustY (sin t2 * radius)
                in
                ( { position = Vec3.vec3 0 0 0, color = color }
                , { position = Vec3.vec3 (cos t1 * radius) y1 0, color = color }
                , { position = Vec3.vec3 (cos t2 * radius) y2 0, color = color }
                )
            )


type alias Uniforms =
    { ucolor : Vec3, view : Mat4, model : Mat4 }


type alias TextureUniforms =
    { ucolor : Vec3, view : Mat4, model : Mat4, texture : Texture }


textureVertexShader : Shader TextureVertex TextureUniforms { vuv : Vec2 }
textureVertexShader =
    [glsl|
attribute vec3 position;
attribute vec2 uv;
varying vec2 vuv;
uniform vec3 ucolor;
uniform mat4 view;
uniform mat4 model;
uniform sampler2D texture;


void main () {
    gl_Position = view * model * vec4(position, 1.0);
    vuv = uv;
}
|]


textureFragmentShader : Shader {} TextureUniforms { vuv : Vec2 }
textureFragmentShader =
    [glsl|
precision mediump float;
varying vec2 vuv;
uniform sampler2D texture;

void main () {
    gl_FragColor = texture2D(texture, vuv);
}
|]


vertexShader : Shader Vertex Uniforms { vcolor : Vec4 }
vertexShader =
    [glsl|
attribute vec3 position;
attribute vec3 color;
varying vec4 vcolor;
uniform vec3 ucolor;
uniform mat4 view;
uniform mat4 model;


void main () {
    gl_Position = view * model * vec4(position, 1.0);

    vcolor = vec4(color.xyz * ucolor, 1.0);
}
|]


fragmentShader : Shader {} Uniforms { vcolor : Vec4 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec4 vcolor;

        void main () {
            gl_FragColor = vcolor;
        }
    |]


backgroundVertexShader : Shader { position : Vec2 } { view : Vec2, viewZoom : Float, windowSize : Vec2 } { worldCoordinate : Vec2 }
backgroundVertexShader =
    [glsl|
attribute vec2 position;
varying vec2 worldCoordinate;
uniform vec2 view;
uniform float viewZoom;
uniform vec2 windowSize;

void main () {
    gl_Position = vec4(position, 0.0, 1.0);

    worldCoordinate = windowSize * position / viewZoom + view * 2.0;
}

|]


backgroundFragmentShader : Shader {} { a | windowSize : Vec2 } { worldCoordinate : Vec2 }
backgroundFragmentShader =
    [glsl|
        precision mediump float;
        varying vec2 worldCoordinate;

        float modI(float a,float b) {
            float m=a-floor((a+0.5)/b)*b;
            return floor(m+0.5);
        }

        void main () {
            float primaryThickness = 5.0;
            float secondaryThickness = 1.0;
            int x0 = modI(worldCoordinate.x + primaryThickness * 0.5, 8.0) <= primaryThickness ? 1 : 0;
            int y0 = modI(worldCoordinate.y + primaryThickness * 0.5, 8.0) <= primaryThickness ? 1 : 0;
            float value = x0 + y0 >= 1 ? 0.96 : 0.98;
            gl_FragColor = vec4(value, value, value, 1.0);
        }
    |]


initMatchData : ServerTime -> Nonempty ( Id UserId, PlayerData ) -> Maybe ( Id FrameId, MatchState ) -> MatchActiveLocal_
initMatchData serverTime newUserIds maybeTimelineCache =
    { timelineCache =
        case maybeTimelineCache of
            Just timelineCache ->
                { cache = List.Nonempty.singleton timelineCache } |> Ok

            Nothing ->
                initMatch serverTime newUserIds |> Timeline.init |> Ok
    , userIds =
        List.Nonempty.toList newUserIds
            |> List.filterMap
                (\( id, playerData ) ->
                    case playerData.mode of
                        PlayerMode ->
                            Just ( id, { skinTone = playerData.skinTone, character = playerData.character } )

                        SpectatorMode ->
                            Nothing
                )
            |> SeqDict.fromList
    , wallMesh = lineSegmentMesh (Vec3.vec3 1 0.7 0.7) wallSegments
    , touchPosition = Nothing
    , previousTouchPosition = Nothing
    , primaryDown = Nothing
    , previousPrimaryDown = Nothing
    , desyncedAtFrame = Nothing
    , footstepMesh = []
    }


updateMatchData :
    Match.Msg
    -> NetworkModel { userId : Id UserId, msg : Match.Msg } Match
    -> NetworkModel { userId : Id UserId, msg : Match.Msg } Match
    -> MatchLocalOnly
    -> MatchLocalOnly
updateMatchData newMsg newNetworkModel oldNetworkModel oldMatchData =
    let
        newMatchState : Match
        newMatchState =
            NetworkModel.localState Match.matchSetupUpdate newNetworkModel

        oldMatchState : Match
        oldMatchState =
            NetworkModel.localState Match.matchSetupUpdate oldNetworkModel
    in
    case ( Match.matchActive newMatchState, Match.matchActive oldMatchState ) of
        ( Just newMatch, Just _ ) ->
            case oldMatchData of
                MatchActiveLocal matchData ->
                    case ( matchData.timelineCache, newMsg ) of
                        ( Ok timelineCache, Match.MatchInputRequest serverTime _ ) ->
                            { matchData
                                | timelineCache =
                                    Timeline.addInput
                                        (Match.serverTimeToFrameId serverTime newMatch)
                                        timelineCache
                            }
                                |> MatchActiveLocal

                        _ ->
                            MatchActiveLocal matchData

                MatchSetupLocal _ ->
                    initMatchData newMatch.startTime (Match.allUsersAndBots newMatchState) Nothing |> MatchActiveLocal

                MatchError ->
                    MatchError

        ( Just newMatch, Nothing ) ->
            initMatchData newMatch.startTime (Match.allUsersAndBots newMatchState) Nothing |> MatchActiveLocal

        ( Nothing, Just _ ) ->
            initMatchSetupData newMatchState |> MatchSetupLocal

        ( Nothing, Nothing ) ->
            oldMatchData


actualTime : Config a -> Time.Posix
actualTime { time, debugTimeOffset } =
    Duration.addTo time debugTimeOffset


initMatch : ServerTime -> Nonempty ( Id UserId, PlayerData ) -> MatchState
initMatch startTime users =
    let
        playerIds =
            List.Nonempty.toList users
                |> List.filterMap
                    (\( userId, playerData ) ->
                        case playerData.mode of
                            PlayerMode ->
                                Just userId

                            SpectatorMode ->
                                Nothing
                    )

        ( shuffledPlayers, _ ) =
            Random.step
                (Random.shuffle playerIds)
                (Match.unwrapServerTime startTime |> Time.posixToMillis |> Random.initialSeed)

        -- Split players into two teams
        halfCount : Int
        halfCount =
            (List.length shuffledPlayers + 1) // 2

        redTeamPlayers : List (Id UserId)
        redTeamPlayers =
            List.take halfCount shuffledPlayers

        blueTeamPlayers : List (Id UserId)
        blueTeamPlayers =
            List.drop halfCount shuffledPlayers
    in
    { players =
        List.map (\userId -> ( userId, initPlayer RedTeam )) redTeamPlayers
            ++ List.map (\userId -> ( userId, initPlayer BlueTeam )) blueTeamPlayers
            |> SeqDict.fromList
            |> initPlayerPosition
    , snowballs = []
    , pushableSnowballs = []
    , particles = []
    , footsteps = []
    , mergedFootsteps = []
    , score = { redTeam = 0, blueTeam = 0 }
    , roundEndTime = Nothing
    , snowballImpacts = []
    }


initPlayerPosition : SeqDict (Id UserId) Player -> SeqDict (Id UserId) Player
initPlayerPosition players =
    let
        spacing =
            Length.inMeters playerRadius * 2.3

        playersPerRow =
            4
    in
    SeqDict.foldr
        (\userId player ( newDict, redCount, blueCount ) ->
            let
                index : Int
                index =
                    case player.team of
                        RedTeam ->
                            redCount

                        BlueTeam ->
                            blueCount

                x : Int
                x =
                    modBy playersPerRow index

                y : Int
                y =
                    index // playersPerRow

                -- For blue team, offset in negative direction (towards center)
                ( xDir, yDir ) =
                    case player.team of
                        RedTeam ->
                            ( 1, 1 )

                        BlueTeam ->
                            ( -1, -1 )

                position : Point2d Meters WorldCoordinate
                position =
                    Point2d.translateBy
                        (Vector2d.fromMeters
                            { x = toFloat x * spacing * xDir
                            , y = toFloat y * spacing * yDir
                            }
                        )
                        (case player.team of
                            RedTeam ->
                                Point2d.meters -12 -8

                            BlueTeam ->
                                Point2d.meters 12 8
                        )
            in
            ( SeqDict.insert
                userId
                { player
                    | position = position
                    , targetPosition = Nothing
                    , velocity = Vector2d.zero
                    , lastStep = { position = position, time = Id.fromInt -999, stepCount = 0 }
                    , isDead = Nothing
                    , rotation =
                        case player.team of
                            RedTeam ->
                                Direction2d.fromAngle (Angle.degrees 45)

                            BlueTeam ->
                                Direction2d.fromAngle (Angle.degrees 225)
                }
                newDict
            , if player.team == RedTeam then
                redCount + 1

              else
                redCount
            , if player.team == BlueTeam then
                blueCount + 1

              else
                blueCount
            )
        )
        ( SeqDict.empty, 0, 0 )
        players
        |> (\( a, _, _ ) -> a)


initPlayer : Team -> Player
initPlayer team =
    { position = Point2d.origin
    , targetPosition = Nothing
    , velocity = Vector2d.zero
    , rotation = Direction2d.x
    , lastEmote = Nothing
    , clickStart = Nothing
    , isDead = Nothing
    , team = team
    , lastStep = { position = Point2d.origin, time = Id.fromInt -999, stepCount = 0 }
    }


point2ToMatrix : Point2d units coordinates -> Mat4
point2ToMatrix point =
    let
        { x, y } =
            Point2d.unwrap point
    in
    Mat4.makeTranslate3 x y 0


point3ToMatrix : Point3d units coordinates -> Mat4
point3ToMatrix point =
    let
        { x, y, z } =
            Point3d.unwrap point
    in
    Mat4.makeTranslate3 x y z


placeToText : Int -> String
placeToText place =
    case place of
        1 ->
            "Winner!"

        2 ->
            "2nd place!"

        3 ->
            "3rd place!"

        21 ->
            "21st place"

        22 ->
            "22nd place"

        23 ->
            "23rd place"

        _ ->
            String.fromInt place ++ "th place"


countdownDelay : Duration
countdownDelay =
    Duration.seconds 4


timeToFrameId : Config a -> MatchActive -> Id FrameId
timeToFrameId model match =
    timeToServerTime model
        |> Match.unwrapServerTime
        |> Duration.from (Match.unwrapServerTime match.startTime)
        |> (\a -> Quantity.ratio a Match.frameDuration)
        |> round
        |> Id.fromInt


timeToServerTime : Config a -> ServerTime
timeToServerTime model =
    pingOffset model |> Duration.addTo (actualTime model) |> ServerTime


pingOffset : { a | pingData : Maybe PingData } -> Duration
pingOffset model =
    case model.pingData of
        Just pingData ->
            Quantity.plus pingData.lowEstimate pingData.highEstimate
                |> Quantity.divideBy 2
                |> Quantity.negate

        Nothing ->
            Quantity.zero


initMatchSetupData : Match -> MatchSetupLocal_
initMatchSetupData lobby =
    let
        preview : LobbyPreview
        preview =
            Match.preview lobby
    in
    { matchName = MatchName.toString preview.name
    , message = ""
    , maxPlayers = String.fromInt preview.maxUserCount
    , botCount = Match.botCount lobby |> String.fromInt
    }


scrollToBottom : Command FrontendOnly toMsg Msg
scrollToBottom =
    Dom.setViewportOf textMessageContainerId 0 99999
        |> Task.attempt (\_ -> ScrolledToBottom)


desyncWarning : Maybe (Id FrameId) -> Ui.Element msg
desyncWarning maybeDesyncFrame =
    case maybeDesyncFrame of
        Just _ ->
            Ui.column
                [ Ui.width Ui.shrink
                , Ui.alignTop
                , Ui.centerX
                , Ui.padding 16
                , Ui.spacing 8
                , Ui.background (Ui.rgba 240 0 0 0.9)
                , Ui.rounded 8
                , Ui.move { x = 0, y = 60, z = 0 }
                , noPointerEvents
                ]
                [ Ui.el
                    [ Ui.width Ui.shrink
                    , Ui.Font.size 20
                    , Ui.Font.bold
                    , Ui.Font.color (Ui.rgb 255 255 255)
                    , Ui.centerX
                    ]
                    (Ui.text "Desync Detected!")
                , Ui.el
                    [ Ui.width Ui.shrink
                    , Ui.Font.size 14
                    , Ui.Font.color (Ui.rgb 255 255 255)
                    , Ui.centerX
                    ]
                    (Ui.text "One or more players have desynced")
                ]

        Nothing ->
            Ui.none


timestamp_ : Duration -> String
timestamp_ difference =
    let
        minutes =
            Duration.inMinutes difference |> floor

        minutesRemainder =
            difference |> Quantity.minus (Duration.minutes (toFloat minutes))

        seconds =
            Duration.inSeconds minutesRemainder |> floor

        secondsRemainder =
            minutesRemainder |> Quantity.minus (Duration.seconds (toFloat seconds))

        milliseconds =
            Duration.inMilliseconds secondsRemainder |> floor
    in
    String.fromInt minutes
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt seconds)
        ++ "."
        ++ String.padLeft 3 '0' (String.fromInt milliseconds)


noPointerEvents =
    Ui.htmlAttribute (Html.Attributes.style "pointer-events" "none")


colorSelector : (ColorIndex -> msg) -> ColorIndex -> Ui.Element msg
colorSelector onSelect currentColor =
    List.Nonempty.toList ColorIndex.allColors
        |> List.map
            (\colorIndex ->
                MyUi.button
                    (colorSelectorHtmlId currentColor)
                    [ Ui.width (Ui.px 36)
                    , Ui.height (Ui.px 36)
                    , Ui.border
                        (if currentColor == colorIndex then
                            3

                         else
                            0
                        )
                    , Ui.borderColor (Ui.rgb 255 255 255)
                    , ColorIndex.toElColor colorIndex |> Ui.background
                    ]
                    { onPress = onSelect colorIndex
                    , label = Ui.none
                    }
            )
        |> Ui.row [ Ui.width Ui.shrink, Ui.wrap ]


colorSelectorHtmlId : ColorIndex -> HtmlId
colorSelectorHtmlId colorIndex =
    "matchPageColorSelector_"
        ++ (case colorIndex of
                Red ->
                    "Red"

                Green ->
                    "Green"

                Blue ->
                    "Blue"

                Orange ->
                    "Orange"

                Brown ->
                    "Brown"

                Purple ->
                    "Purple"

                Pink ->
                    "Pink"

                Yellow ->
                    "Yellow"
           )
        |> Dom.id


skinToneSelector : (SkinTone -> msg) -> SkinTone -> Ui.Element msg
skinToneSelector onSelect currentSkinTone =
    List.Nonempty.toList SkinTone.allSkinTones
        |> List.map
            (\skinTone ->
                MyUi.button
                    (skinToneSelectorHtmlId currentSkinTone)
                    [ Ui.width (Ui.px 36)
                    , Ui.height (Ui.px 36)
                    , Ui.border
                        (if currentSkinTone == skinTone then
                            3

                         else
                            0
                        )
                    , Ui.borderColor (Ui.rgb 255 255 255)
                    , SkinTone.toElColor skinTone |> Ui.background
                    ]
                    { onPress = onSelect skinTone
                    , label = Ui.none
                    }
            )
        |> Ui.row [ Ui.width Ui.shrink, Ui.wrap ]


skinToneSelectorHtmlId : SkinTone -> HtmlId
skinToneSelectorHtmlId skinTone =
    "matchPageSkinToneSelector_"
        ++ (case skinTone of
                SkinTone.Light ->
                    "Light"

                SkinTone.Fair ->
                    "Fair"

                SkinTone.Medium ->
                    "Medium"

                SkinTone.Tan ->
                    "Tan"

                SkinTone.Brown ->
                    "Brown"

                SkinTone.Dark ->
                    "Dark"
           )
        |> Dom.id


viewportHeight : Length
viewportHeight =
    Length.meters 25


getInput : Config a -> MatchActiveLocal_ -> Input
getInput config model =
    { action =
        case ( model.primaryDown, model.previousPrimaryDown ) of
            ( Just _, Nothing ) ->
                case model.touchPosition of
                    Just position ->
                        screenToWorld config.windowSize Point2d.origin viewportHeight position |> ClickStart

                    _ ->
                        NoAction

            ( Nothing, Just _ ) ->
                case model.touchPosition of
                    Just position ->
                        screenToWorld config.windowSize Point2d.origin viewportHeight position |> ClickRelease

                    _ ->
                        NoAction

            _ ->
                NoAction
    , emote =
        if Keyboard.keyPressed config (Keyboard.Character "1") then
            Just SurpriseEmote

        else if Keyboard.keyPressed config (Keyboard.Character "2") then
            Just ImpEmote

        else
            Nothing
    }


noInput : Input
noInput =
    { action = NoAction, emote = Nothing }


animationFrame : Config a -> Model -> ( Model, Command FrontendOnly ToBackend Msg )
animationFrame config model =
    case ( model.matchData, Match.matchActive (getLocalState model) ) of
        ( MatchActiveLocal matchData, Just match ) ->
            case matchData.timelineCache of
                Ok cache ->
                    case Timeline.getStateAt gameUpdate (timeToFrameId config match) cache match.timeline of
                        Ok ( newCache, matchState ) ->
                            let
                                input : Input
                                input =
                                    getInput config matchData

                                model3 : Model
                                model3 =
                                    { model
                                        | matchData =
                                            { matchData
                                                | previousTouchPosition = matchData.touchPosition
                                                , previousPrimaryDown = matchData.primaryDown
                                                , timelineCache = Ok newCache
                                            }
                                                |> MatchActiveLocal
                                    }

                                ( oldestFrameId, oldestState ) =
                                    getOldestCachedState newCache

                                playerPositionsCmd : Command FrontendOnly ToBackend msg
                                playerPositionsCmd =
                                    if modBy 2 (Id.toInt oldestFrameId) == 0 && Env.isProduction then
                                        DesyncCheckRequest
                                            model.lobbyId
                                            oldestFrameId
                                            (SeqDict.map (\_ player -> player.position) oldestState.players)
                                            |> Effect.Lamdera.sendToBackend

                                    else
                                        Command.none
                            in
                            (if noInput == input then
                                ( model3, playerPositionsCmd )

                             else
                                matchSetupUpdate
                                    config.userId
                                    (Match.MatchInputRequest (timeToServerTime config) input)
                                    model3
                                    |> Tuple.mapSecond (\cmd -> Command.batch [ cmd, playerPositionsCmd ])
                            )
                                |> (\( matchSetupPage2, cmd ) ->
                                        let
                                            maybeWinner =
                                                case ( matchState.score.redTeam > 2, matchState.score.blueTeam > 2 ) of
                                                    ( True, True ) ->
                                                        Just BothWon

                                                    ( True, False ) ->
                                                        Just RedWon

                                                    ( False, True ) ->
                                                        Just BlueWon

                                                    ( False, False ) ->
                                                        Nothing
                                        in
                                        case maybeWinner of
                                            Just winner ->
                                                matchSetupUpdate config.userId (Match.MatchFinished winner) matchSetupPage2
                                                    |> Tuple.mapSecond (\cmd2 -> Command.batch [ cmd, cmd2, scrollToBottom ])

                                            Nothing ->
                                                ( matchSetupPage2, cmd )
                                   )

                        Err _ ->
                            ( model, Command.none )

                Err _ ->
                    ( model, Command.none )

        _ ->
            ( model, Command.none )


frameTimeElapsed : Id FrameId -> Id FrameId -> Duration
frameTimeElapsed start end =
    Quantity.multiplyBy (toFloat (Id.toInt end - Id.toInt start)) Match.frameDuration


textMessageContainerId : HtmlId
textMessageContainerId =
    Dom.id "textMessageContainer"


getLocalState : Model -> Match
getLocalState matchPage =
    NetworkModel.localState Match.matchSetupUpdate matchPage.networkModel


audio : Config a -> Model -> Audio
audio loaded matchPage =
    case ( Match.matchActive (getLocalState matchPage), matchPage.matchData ) of
        ( Just match, MatchActiveLocal matchData ) ->
            case matchData.timelineCache of
                Ok cache ->
                    let
                        currentFrameId =
                            timeToFrameId loaded match
                    in
                    case Timeline.getStateAt gameUpdate currentFrameId cache match.timeline of
                        Ok ( _, state ) ->
                            let
                                frameToTime : Id FrameId -> Time.Posix
                                frameToTime frameId =
                                    Quantity.multiplyBy (Id.toInt frameId |> toFloat) Match.frameDuration
                                        |> Duration.addTo (Match.unwrapServerTime match.startTime)
                                        |> (\a -> Duration.subtractFrom a (pingOffset loaded))
                                        |> (\a -> Duration.subtractFrom a loaded.debugTimeOffset)

                                chargeSounds : List Audio
                                chargeSounds =
                                    List.filterMap
                                        (\player ->
                                            case player.clickStart of
                                                Just clickStart ->
                                                    let
                                                        sound =
                                                            if modBy 2 (Id.toInt clickStart.time) == 0 then
                                                                loaded.sounds.charge

                                                            else
                                                                loaded.sounds.charge2
                                                    in
                                                    Audio.audio sound (frameToTime clickStart.time) |> Just

                                                Nothing ->
                                                    Nothing
                                        )
                                        (SeqDict.values state.players)

                                footstepSounds : Audio
                                footstepSounds =
                                    List.map
                                        (\( userId, player ) ->
                                            Audio.audio
                                                (footstepSound loaded userId player.lastStep.time)
                                                (frameToTime player.lastStep.time)
                                        )
                                        (SeqDict.toList state.players)
                                        |> Audio.group
                                        |> Audio.scaleVolume 0.1

                                deadSounds : List Audio
                                deadSounds =
                                    List.filterMap
                                        (\( userId, player ) ->
                                            case player.isDead of
                                                Just isDead ->
                                                    Audio.audio
                                                        (deadSound loaded userId)
                                                        (frameToTime isDead.time)
                                                        |> Just

                                                Nothing ->
                                                    Nothing
                                        )
                                        (SeqDict.toList state.players)

                                throwSounds : List Audio
                                throwSounds =
                                    List.map
                                        (\snowball ->
                                            Audio.audio
                                                (throwSound loaded snowball.thrownAt)
                                                (frameToTime snowball.thrownAt)
                                        )
                                        state.snowballs

                                whooshSounds : List Audio
                                whooshSounds =
                                    List.filterMap
                                        (\snowball ->
                                            case snowball.apexFrame of
                                                Just apexFrameId ->
                                                    Audio.audio loaded.sounds.whoosh (frameToTime apexFrameId) |> Just

                                                Nothing ->
                                                    Nothing
                                        )
                                        state.snowballs

                                countdownSounds : List Audio
                                countdownSounds =
                                    let
                                        sounds =
                                            Random.step
                                                (Random.shuffle
                                                    [ loaded.sounds.pop
                                                    , loaded.sounds.railToggle
                                                    , loaded.sounds.erase
                                                    ]
                                                )
                                                (Random.initialSeed (Time.posixToMillis (Match.unwrapServerTime match.startTime)))
                                                |> Tuple.first
                                                |> List.take 3
                                    in
                                    List.indexedMap
                                        (\index sound ->
                                            Audio.audio
                                                sound
                                                (frameToTime (Id.fromInt ((index + 1) * Match.framesPerSecond)))
                                        )
                                        (sounds ++ [ loaded.sounds.meow ])

                                pointAdded : List Audio
                                pointAdded =
                                    case state.roundEndTime of
                                        Just roundEnd ->
                                            [ Audio.audio
                                                loaded.sounds.lightSwitch
                                                (Duration.addTo (frameToTime roundEnd.time) (Duration.seconds 1))
                                            ]

                                        Nothing ->
                                            []

                                hitSounds : Audio
                                hitSounds =
                                    List.map
                                        (\impactTime ->
                                            Audio.audioWithConfig
                                                { loop = Nothing, playbackRate = 1.5, startAt = Quantity.zero }
                                                loaded.sounds.footstep4
                                                (frameToTime impactTime)
                                        )
                                        state.snowballImpacts
                                        |> Audio.group
                                        |> Audio.scaleVolume 0.5
                            in
                            footstepSounds
                                :: hitSounds
                                :: chargeSounds
                                ++ deadSounds
                                ++ throwSounds
                                ++ whooshSounds
                                ++ countdownSounds
                                ++ pointAdded
                                |> Audio.group

                        Err _ ->
                            Audio.silence

                Err _ ->
                    Audio.silence

        _ ->
            Audio.silence


footstepSound : Config a -> Id UserId -> Id FrameId -> Audio.Source
footstepSound loaded userId frameId =
    case modBy 8 (Id.toInt frameId + Id.toInt userId * 7) of
        0 ->
            loaded.sounds.footstep1

        1 ->
            loaded.sounds.footstep2

        2 ->
            loaded.sounds.footstep3

        3 ->
            loaded.sounds.footstep4

        4 ->
            loaded.sounds.footstep5

        5 ->
            loaded.sounds.footstep6

        6 ->
            loaded.sounds.footstep7

        _ ->
            loaded.sounds.footstep8


deadSound : Config a -> Id UserId -> Audio.Source
deadSound loaded userId =
    case modBy 5 (Id.toInt userId) of
        0 ->
            loaded.sounds.dead1

        1 ->
            loaded.sounds.dead2

        2 ->
            loaded.sounds.dead3

        3 ->
            loaded.sounds.dead4

        _ ->
            loaded.sounds.dead5


throwSound : Config a -> Id FrameId -> Audio.Source
throwSound loaded frameId =
    if modBy 2 (Id.toInt frameId) == 0 then
        loaded.sounds.throw1

    else
        loaded.sounds.throw2
