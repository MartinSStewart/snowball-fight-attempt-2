module Textures exposing (CharacterTextures, Textures, loadingFinished, requestTextures, textureOptions)

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
    , grumble0 : Texture
    , grumble1 : Texture
    , grumble2 : Texture
    , grumble3 : Texture
    , grumble4 : Texture
    , grumble5 : Texture
    , bones : CharacterTextures
    , charlotte : CharacterTextures
    , bot : CharacterTextures
    , stana : CharacterTextures
    , knifery : CharacterTextures
    , dael : CharacterTextures
    , tanis : CharacterTextures
    }


type alias CharacterTextures =
    { base : Texture
    , shadows : Texture
    , eye : Texture
    , arms : Texture
    , shadowArms : Texture
    }


textureUrls : List String
textureUrls =
    [ "/vignette.png"
    , "/video0.png"
    , "/video1.png"
    , "/video2.png"
    , "/grumble0.png"
    , "/grumble1.png"
    , "/grumble2.png"
    , "/grumble3.png"
    , "/grumble4.png"
    , "/grumble5.png"
    ]
        ++ List.concatMap
            (\a ->
                let
                    name =
                        Character.folderName a
                in
                [ "/" ++ name ++ "/base.png"
                , "/" ++ name ++ "/shadow.png"
                , "/" ++ name ++ "/eye.png"
                , "/" ++ name ++ "/arms.png"
                , "/" ++ name ++ "/shadow_arms.png"
                ]
            )
            Character.all


requestTextures : (String -> Result Texture.Error Texture -> msg) -> Command FrontendOnly toBackend msg
requestTextures loadedTexture =
    List.map
        (\url ->
            Texture.loadWith textureOptions url
                |> Task.attempt identity
                |> Command.map identity (loadedTexture url)
        )
        textureUrls
        |> Command.batch


textureOptions : Texture.Options
textureOptions =
    { magnify = Texture.nearest
    , minify = Texture.linear
    , horizontalWrap = Texture.clampToEdge
    , verticalWrap = Texture.clampToEdge
    , flipY = True
    , premultiplyAlpha = True
    }


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
                        |> loadTexture
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
        |> loadTexture
        |> loadTexture
        |> loadTexture
        |> loadTexture
        |> loadTexture
        |> loadTexture
        |> loadCharacterTexture
        |> loadCharacterTexture
        |> loadCharacterTexture
        |> loadCharacterTexture
        |> loadCharacterTexture
        |> loadCharacterTexture
        |> loadCharacterTexture
        |> Tuple.second
