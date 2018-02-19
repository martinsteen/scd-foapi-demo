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
    | Mdl (Material.Msg Msg)
    | RemoveEndpoint Endpoint
    | CommitEdit Endpoint (Maybe String)
    | StartEdit Endpoint
    | StartAdd
    | CancelEdit
    | UpdateEdit ( EndpointEditor.Field, EndpointEditor.FieldContent )
