module User exposing
    ( BackendUser
    , UserId
    , getUser
    )

import Id exposing (Id)
import SeqDict exposing (SeqDict)


type UserId
    = UserId Never


type alias BackendUser =
    { name : String }


getUser : Id UserId -> { a | userId : Id UserId, currentUser : BackendUser, users : SeqDict (Id UserId) BackendUser } -> Maybe BackendUser
getUser userId record =
    if record.userId == userId then
        Just record.currentUser

    else
        SeqDict.get userId record.users
