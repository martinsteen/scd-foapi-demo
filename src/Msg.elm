module Msg exposing (..)

import Dropbox
import Material
import Endpoint

type alias Endpoint =
    Endpoint.Endpoint

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
    | RemoveEndpoint Endpoint
    | CommitEdit Endpoint (Maybe String)
    | StartEdit Endpoint
    | StartAdd
    | CancelEdit
    | UpdateEdit ( Field, String )
