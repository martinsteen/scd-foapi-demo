module Model exposing (..)

import Material
import Dropbox
import Navigation
import Endpoint

type alias Storage =
    { endpoints : List Endpoint }

type alias Endpoint =
    Endpoint.Endpoint
    
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

