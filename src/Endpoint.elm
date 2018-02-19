module Endpoint exposing (..)


type alias Endpoint =
    { name : String
    , url : String
    , alerts : List Int
    , user : String
    , password : String
    }
