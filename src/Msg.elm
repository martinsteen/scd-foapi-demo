module Msg exposing (..)

import Dropbox
import Material
import Endpoint
import EndpointEditor


type alias Endpoint =
    Endpoint.Endpoint


type Msg
    = LogInToDropbox
    | AuthResponse Dropbox.AuthorizeResult
    | FetchFileResponse (Result Dropbox.DownloadError Dropbox.DownloadResponse)
    | PutFileReponse (Result Dropbox.UploadError Dropbox.UploadResponse)
    | RemoveEndpoint Endpoint
    | StartEdit Endpoint
    | StartAdd
    | Mdl (Material.Msg Msg)
    | EpEdit (EndpointEditor.Msg Msg)
