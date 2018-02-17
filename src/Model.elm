module Model exposing (..)

import Material
import Dropbox
import Navigation
import Storage exposing (Storage)
import EndpointEditor exposing (Model)


type alias Model =
    { storage : Storage.Storage
    , location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    , error : Maybe String
    , mdl : Material.Model
    , editor : Maybe EndpointEditor.Model
    }
