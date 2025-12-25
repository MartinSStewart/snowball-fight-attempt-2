module Character exposing
    ( Character(..)
    , all
    , toString
    , fromString
    , toPortraitPath
    , codec
    )

import Serialize


type Character
    = Character1
    | Character2
    | Character3
    | Character4
    | Character5
    | Character6
    | Character7
    | Character8
    | Character9
    | Character10


all : List Character
all =
    [ Character1
    , Character2
    , Character3
    , Character4
    , Character5
    , Character6
    , Character7
    , Character8
    , Character9
    , Character10
    ]


toString : Character -> String
toString character =
    case character of
        Character1 ->
            "Character1"

        Character2 ->
            "Character2"

        Character3 ->
            "Character3"

        Character4 ->
            "Character4"

        Character5 ->
            "Character5"

        Character6 ->
            "Character6"

        Character7 ->
            "Character7"

        Character8 ->
            "Character8"

        Character9 ->
            "Character9"

        Character10 ->
            "Character10"


fromString : String -> Maybe Character
fromString str =
    case str of
        "Character1" ->
            Just Character1

        "Character2" ->
            Just Character2

        "Character3" ->
            Just Character3

        "Character4" ->
            Just Character4

        "Character5" ->
            Just Character5

        "Character6" ->
            Just Character6

        "Character7" ->
            Just Character7

        "Character8" ->
            Just Character8

        "Character9" ->
            Just Character9

        "Character10" ->
            Just Character10

        _ ->
            Nothing


toPortraitPath : Character -> String
toPortraitPath character =
    case character of
        Character1 ->
            "/character1.png"

        Character2 ->
            "/character2.png"

        Character3 ->
            "/character3.png"

        Character4 ->
            "/character4.png"

        Character5 ->
            "/character5.png"

        Character6 ->
            "/character6.png"

        Character7 ->
            "/character7.png"

        Character8 ->
            "/character8.png"

        Character9 ->
            "/character9.png"

        Character10 ->
            "/character10.png"


codec : Serialize.Codec e Character
codec =
    Serialize.customType
        (\character1 character2 character3 character4 character5 character6 character7 character8 character9 character10 value ->
            case value of
                Character1 ->
                    character1

                Character2 ->
                    character2

                Character3 ->
                    character3

                Character4 ->
                    character4

                Character5 ->
                    character5

                Character6 ->
                    character6

                Character7 ->
                    character7

                Character8 ->
                    character8

                Character9 ->
                    character9

                Character10 ->
                    character10
        )
        |> Serialize.variant0 Character1
        |> Serialize.variant0 Character2
        |> Serialize.variant0 Character3
        |> Serialize.variant0 Character4
        |> Serialize.variant0 Character5
        |> Serialize.variant0 Character6
        |> Serialize.variant0 Character7
        |> Serialize.variant0 Character8
        |> Serialize.variant0 Character9
        |> Serialize.variant0 Character10
        |> Serialize.finishCustomType
