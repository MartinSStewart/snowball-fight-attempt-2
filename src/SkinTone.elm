module SkinTone exposing (SkinTone(..), allSkinTones, toElColor, toVec3)

import List.Nonempty exposing (Nonempty(..))
import Math.Vector3 exposing (Vec3)
import Ui


type SkinTone
    = Light
    | Fair
    | Medium
    | Tan
    | Brown
    | Dark


toVec3 : SkinTone -> Vec3
toVec3 skinTone =
    case skinTone of
        Light ->
            Math.Vector3.vec3 1 0.92 0.8

        Fair ->
            Math.Vector3.vec3 1 0.87 0.73

        Medium ->
            Math.Vector3.vec3 1 0.8 0.5

        Tan ->
            Math.Vector3.vec3 0.92 0.7 0.46

        Brown ->
            Math.Vector3.vec3 0.65 0.45 0.3

        Dark ->
            Math.Vector3.vec3 0.4 0.28 0.2


allSkinTones : Nonempty SkinTone
allSkinTones =
    Nonempty Light [ Fair, Medium, Tan, Brown, Dark ]


toElColor : SkinTone -> Ui.Color
toElColor skinTone =
    let
        vec =
            toVec3 skinTone

        r =
            Math.Vector3.getX vec

        g =
            Math.Vector3.getY vec

        b =
            Math.Vector3.getZ vec
    in
    Ui.rgb (round (r * 255)) (round (g * 255)) (round (b * 255))
