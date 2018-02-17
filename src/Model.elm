module Model exposing (..)

import Material
import Dropbox
import Navigation
import Endpoint
import Storage exposing (Storage)

type alias Endpoint =
    Endpoint.Endpoint

type alias Storage =
    Storage.Storage

type alias Model =
    { storage : Storage
    , location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    , error : Maybe String
    , mdl : Material.Model
    , editor : Maybe EndpointEditorModel
    }

type alias EndpointEditorModel =
    { endpoint : Endpoint
    , id : Maybe String
    }

