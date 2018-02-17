module Model exposing (..)

import Material
import Dropbox
import Navigation


type alias Storage =
    { endpoints : List Endpoint }


type alias Endpoint =
    { name : String
    , url : String
    , alerts : List Int
    , user : String
    , password : String
    }


type alias Model =
    { storage : Storage
    , location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    , error : Maybe String
    , mdl : Material.Model
    , endpointUnderConstruction : Maybe Endpoint
    , defaultEndpoint : Endpoint
    }
