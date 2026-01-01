module Evergreen.V7.Textures exposing (..)

import Effect.WebGL.Texture


type alias CharacterTextures =
    { base : Effect.WebGL.Texture.Texture
    , shadows : Effect.WebGL.Texture.Texture
    , eye : Effect.WebGL.Texture.Texture
    , arms : Effect.WebGL.Texture.Texture
    , shadowArms : Effect.WebGL.Texture.Texture
    }


type alias Textures =
    { vignette : Effect.WebGL.Texture.Texture
    , video0 : Effect.WebGL.Texture.Texture
    , video1 : Effect.WebGL.Texture.Texture
    , video2 : Effect.WebGL.Texture.Texture
    , grumble0 : Effect.WebGL.Texture.Texture
    , grumble1 : Effect.WebGL.Texture.Texture
    , grumble2 : Effect.WebGL.Texture.Texture
    , grumble3 : Effect.WebGL.Texture.Texture
    , grumble4 : Effect.WebGL.Texture.Texture
    , grumble5 : Effect.WebGL.Texture.Texture
    , bones : CharacterTextures
    , charlotte : CharacterTextures
    , bot : CharacterTextures
    , stana : CharacterTextures
    , knifery : CharacterTextures
    , dael : CharacterTextures
    , tanis : CharacterTextures
    , vael : CharacterTextures
    , roland : CharacterTextures
    , eden : CharacterTextures
    , crow : CharacterTextures
    , emiko : CharacterTextures
    , dahlia : CharacterTextures
    }
