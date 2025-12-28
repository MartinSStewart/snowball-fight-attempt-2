module Character exposing
    ( Character(..)
    , all
    , fromString
    , toString
    )


type Character
    = Bones
    | Charlotte


all : List Character
all =
    [ Bones
    , Charlotte
    ]


toString : Character -> String
toString character =
    case character of
        Bones ->
            "bones"

        Charlotte ->
            "charlotte"


fromString : String -> Maybe Character
fromString str =
    case str of
        "Character1" ->
            Just Bones

        "Character2" ->
            Just Charlotte

        _ ->
            Nothing
