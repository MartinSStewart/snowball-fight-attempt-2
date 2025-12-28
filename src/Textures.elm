module Textures exposing (Textures, loadingFinished, requestTextures)

import Character exposing (Character)
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Task as Task exposing (Task)
import Effect.WebGL.Texture as Texture exposing (Texture)
import SeqDict exposing (SeqDict)


type alias Textures =
    { vignette : Texture
    , video0 : Texture
    , video1 : Texture
    , video2 : Texture
    , bones : CharacterTextures
    , charlotte : CharacterTextures
    }


type alias CharacterTextures =
    { base : Texture
    , shadows : Texture
    }


textureUrls : List String
textureUrls =
    [ "/vignette.png"
    , "/video0.png"
    , "/video1.png"
    , "/video2.png"
    ]
        ++ List.concatMap
            (\a ->
                let
                    name =
                        Character.toString a
                in
                [ "/" ++ name ++ "/base.png"
                , "/" ++ name ++ "/shadow.png"
                ]
            )
            Character.all


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
                , premultiplyAlpha = True
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
        loadTexture : ( List String, Maybe (Texture -> b) ) -> ( List String, Maybe b )
        loadTexture ( urlsLeft, soundsFinished ) =
            case ( urlsLeft, soundsFinished ) of
                ( head :: rest, Just soundsFinished_ ) ->
                    case SeqDict.get head sounds of
                        Just (Ok source) ->
                            ( rest, soundsFinished_ source |> Just )

                        _ ->
                            ( [], Nothing )

                _ ->
                    ( [], Nothing )

        loadCharacterTexture : ( List String, Maybe (CharacterTextures -> b) ) -> ( List String, Maybe b )
        loadCharacterTexture ( urlsLeft, maybeTextures ) =
            case maybeTextures of
                Just textures ->
                    ( urlsLeft, Just CharacterTextures )
                        |> loadTexture
                        |> loadTexture
                        |> (\( a, b ) ->
                                ( a
                                , case b of
                                    Just b2 ->
                                        textures b2 |> Just

                                    Nothing ->
                                        Nothing
                                )
                           )

                Nothing ->
                    ( urlsLeft, Nothing )
    in
    ( textureUrls, Just Textures )
        |> loadTexture
        |> loadTexture
        |> loadTexture
        |> loadTexture
        |> loadCharacterTexture
        |> loadCharacterTexture
        |> Tuple.second
