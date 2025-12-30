module Evergreen.V2.Textures exposing (..)

import Effect.WebGL.Texture


type alias CharacterTextures =
    { base : Effect.WebGL.Texture.Texture
    , shadows : Effect.WebGL.Texture.Texture
    , eye : Effect.WebGL.Texture.Texture
    }


type alias Textures =
    { vignette : Effect.WebGL.Texture.Texture
    , video0 : Effect.WebGL.Texture.Texture
    , video1 : Effect.WebGL.Texture.Texture
    , video2 : Effect.WebGL.Texture.Texture
    , bones : CharacterTextures
    , charlotte : CharacterTextures
    }
