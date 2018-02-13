module Model exposing (..)

import Material
import Dropbox exposing (..)
import Navigation


type alias Endpoint =
    { name : String
    , url : String
    , alerts : List Int
    }


type alias Storage =
    { endpoints : List Endpoint }


type alias Model =
    { storage : Storage
    , location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    , error : Maybe String
    , mdl : Material.Model
    }
