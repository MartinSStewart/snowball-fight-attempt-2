module Textures exposing (Textures, loadingFinished, requestTextures)

import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Task as Task exposing (Task)
import Effect.WebGL.Texture as Texture exposing (Texture)
import SeqDict exposing (SeqDict)


type alias Textures =
    { vignette : Texture
    , video0 : Texture
    , video1 : Texture
    , video2 : Texture
    }


textureUrls : List String
textureUrls =
    [ "/vignette.png"
    , "/video0.png"
    , "/video1.png"
    , "/video2.png"
    ]


requestTextures : (String -> Result Texture.Error Texture -> msg) -> Command FrontendOnly toBackend msg
requestTextures loadedTexture =
    List.map
        (\url ->
            Texture.loadWith
                { magnify = Texture.linear
                , minify = Texture.linear
                , horizontalWrap = Texture.clampToEdge
                , verticalWrap = Texture.clampToEdge
                , flipY = False
                , premultiplyAlpha = False
                }
                url
                |> Task.attempt identity
                |> Command.map identity (loadedTexture url)
        )
        textureUrls
        |> Command.batch


loadingFinished : SeqDict String (Result Texture.Error Texture) -> Maybe Textures
loadingFinished sounds =
    let
        loadSound : ( List String, Maybe (Texture -> b) ) -> ( List String, Maybe b )
        loadSound ( urlsLeft, soundsFinished ) =
            case ( urlsLeft, soundsFinished ) of
                ( head :: rest, Just soundsFinished_ ) ->
                    case SeqDict.get head sounds of
                        Just (Ok source) ->
                            ( rest, soundsFinished_ source |> Just )

                        _ ->
                            ( [], Nothing )

                _ ->
                    ( [], Nothing )
    in
    ( textureUrls, Just Textures )
        |> loadSound
        |> loadSound
        |> loadSound
        |> loadSound
        |> Tuple.second
