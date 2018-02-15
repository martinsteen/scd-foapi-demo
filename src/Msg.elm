module Msg exposing (..)

import Dropbox
import Material
import Model


type alias Endpoint =
    Model.Endpoint


type Field
    = Name
    | User
    | Url
    | Password


type Msg
    = LogInToDropbox
    | AuthResponse Dropbox.AuthorizeResult
    | FetchFileResponse (Result Dropbox.DownloadError Dropbox.DownloadResponse)
    | PutFileReponse (Result Dropbox.UploadError Dropbox.UploadResponse)
    | Mdl (Material.Msg Msg)
    | EditEndpoint Endpoint
    | CancelEdit
    | RemoveEndpoint Endpoint
    | SaveEndpoint Endpoint
    | UpdateEndpoinUnderConstruction ( Field, String )
