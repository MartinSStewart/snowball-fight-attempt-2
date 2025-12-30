module Character exposing
    ( Character(..)
    , all
    , toString
    )


type Character
    = Bones
    | Charlotte
    | Bot


all : List Character
all =
    [ Bones
    , Charlotte
    , Bot
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
