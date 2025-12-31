module Character exposing
    ( Character(..)
    , all
    , eyeColor
    , folderName
    , name
    , skinTone
    )

import Math.Vector3 as Vec3 exposing (Vec3)


type Character
    = Bones
    | Sheire
    | Bot
    | Stana
    | Knifery
    | Dael
    | Tanis
    | Vael
    | Roland
    | Eden


all : List Character
all =
    [ Bones
    , Sheire
    , Bot
    , Stana
    , Knifery
    , Dael
    , Tanis
    , Vael
    , Roland
    , Eden
    ]


folderName : Character -> String
folderName character =
    case character of
        Bones ->
            "/bones"

        Sheire ->
            "/sheire"

        Bot ->
            "/bot"

        Stana ->
            "/stana"

        Knifery ->
            "/knifery"

        Dael ->
            "/dael"

        Tanis ->
            "/tanis"

        Vael ->
            "/vael"

        Roland ->
            "/roland"

        Eden ->
            "/eden"


name : Character -> String
name character =
    case character of
        Bones ->
            "Bones"

        Sheire ->
            "Sheire"

        Bot ->
            "AT2"

        Stana ->
            "Stana"

        Knifery ->
            "Knifery"

        Dael ->
            "Dael"

        Tanis ->
            "Tanis"

        Vael ->
            "Vael"

        Roland ->
            "Roland"

        Eden ->
            "Eden"


skinTone : Character -> Vec3
skinTone character =
    case character of
        Bones ->
            Vec3.vec3 0.78 0.77 0.49

        Sheire ->
            Vec3.vec3 0.45 0.31 0.17

        Bot ->
            Vec3.vec3 0.6 0.88 0.58

        Stana ->
            Vec3.vec3 0.82 0.51 0.46

        Knifery ->
            Vec3.vec3 0 0 0

        Dael ->
            Vec3.vec3 0.46 0.26 0.11

        Tanis ->
            Vec3.vec3 0.89 0.8 0.74

        Vael ->
            Vec3.vec3 0.84 0.75 0.73

        Roland ->
            Vec3.vec3 1 1 1

        Eden ->
            Vec3.vec3 0.7 0.7 0.7


eyeColor : Character -> Vec3
eyeColor character =
    case character of
        Bones ->
            Vec3.vec3 0 0 0

        Sheire ->
            Vec3.vec3 0 0 0

        Bot ->
            Vec3.vec3 0 0 0

        Stana ->
            Vec3.vec3 0.95 0.86 0.76

        Knifery ->
            Vec3.vec3 1 1 1

        Dael ->
            Vec3.vec3 0.31 0.84 0.84

        Tanis ->
            Vec3.vec3 0 0 0

        Vael ->
            Vec3.vec3 0 0 0

        Roland ->
            Vec3.vec3 0 0 0

        Eden ->
            Vec3.vec3 0 0 0



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
