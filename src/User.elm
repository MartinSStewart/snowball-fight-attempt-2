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


getUser : { a | userId : Id UserId, currentUser : BackendUser, users : SeqDict (Id UserId) BackendUser } -> Id UserId -> Maybe BackendUser
getUser record userId =
    if record.userId == userId then
        Just record.currentUser

    else
        SeqDict.get userId record.users
