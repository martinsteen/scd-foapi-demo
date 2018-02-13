module Model exposing (..)

import Material
import Dropbox
import Navigation
import Storage


type alias Storage =
    Storage.Storage


type alias Model =
    { storage : Storage
    , location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    , error : Maybe String
    , mdl : Material.Model
    }
