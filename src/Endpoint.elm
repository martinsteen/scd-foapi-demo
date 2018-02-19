module Endpoint exposing (..)


type alias Endpoint =
    { name : String
    , url : String
    , alerts : List Int
    , user : String
    , password : String
    }

type Field
    = Name
    | User
    | Url
    | Password

type FieldInput
    = Error String
    | Value String
