module Character exposing
    ( Character(..)
    , all
    , toString
    )


type Character
    = Bones
    | Charlotte
    | Bot
    | Stana


all : List Character
all =
    [ Bones
    , Charlotte
    , Bot
    , Stana
    ]


toString : Character -> String
toString character =
    case character of
        Bones ->
            "bones"

        Charlotte ->
            "charlotte"

        Bot ->
            "bot"

        Stana ->
            "stana"
