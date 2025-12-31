module Character exposing
    ( Character(..)
    , all
    , eyeColor
    , folderName
    , skinTone
    )

import Math.Vector3 as Vec3 exposing (Vec3)


type Character
    = Bones
    | Charlotte
    | Bot
    | Stana
    | Knifery
    | Dael


all : List Character
all =
    [ Bones
    , Charlotte
    , Bot
    , Stana
    , Knifery
    , Dael
    ]


folderName : Character -> String
folderName character =
    case character of
        Bones ->
            "bones"

        Charlotte ->
            "charlotte"

        Bot ->
            "bot"

        Stana ->
            "stana"

        Knifery ->
            "knifery"

        Dael ->
            "dael"


skinTone : Character -> Vec3
skinTone character =
    case character of
        Bones ->
            Vec3.vec3 0.78 0.77 0.49

        Charlotte ->
            Vec3.vec3 0.45 0.31 0.17

        Bot ->
            Vec3.vec3 0.6 0.88 0.58

        Stana ->
            Vec3.vec3 0.82 0.51 0.46

        Knifery ->
            Vec3.vec3 0 0 0

        Dael ->
            Vec3.vec3 0.46 0.26 0.11


eyeColor : Character -> Vec3
eyeColor character =
    case character of
        Bones ->
            Vec3.vec3 0 0 0

        Charlotte ->
            Vec3.vec3 0 0 0

        Bot ->
            Vec3.vec3 0 0 0

        Stana ->
            Vec3.vec3 0.95 0.86 0.76

        Knifery ->
            Vec3.vec3 1 1 1

        Dael ->
            Vec3.vec3 0.31 0.84 0.84



--toVec3 : SkinTone -> Vec3
--toVec3 skinTone =
--    case skinTone of
--        Light ->
--            Math.Vector3.vec3 1 0.92 0.8
--
--        Fair ->
--            Math.Vector3.vec3 1 0.87 0.73
--
--        Medium ->
--            Math.Vector3.vec3 1 0.8 0.5
--
--        Tan ->
--            Math.Vector3.vec3 0.92 0.7 0.46
--
--        Brown ->
--            Math.Vector3.vec3 0.65 0.45 0.3
--
--        Dark ->
--            Math.Vector3.vec3 0.4 0.28 0.2
