module Storage exposing (Storage, decodeStorage, uploadCmd, downloadCmd)

import Json.Encode exposing (Value, object, string)
import Json.Decode exposing (int, string, float, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Dropbox
import Task
import Date
import Endpoint


type alias Endpoint =
    Endpoint.Endpoint


type alias Storage =
    { endpoints : List Endpoint }


downloadCmd : Dropbox.UserAuth -> Task.Task Dropbox.DownloadError Dropbox.DownloadResponse
downloadCmd auth =
    Dropbox.download auth
        { path = "/endpoint-data.json" }


uploadCmd : Dropbox.UserAuth -> Storage -> Task.Task Dropbox.UploadError Dropbox.UploadResponse
uploadCmd auth storage =
    storage
        |> encodeStorage
        |> createUploadReq
        |> Dropbox.upload auth


createUploadReq : String -> Dropbox.UploadRequest
createUploadReq content =
    Dropbox.UploadRequest
        "/endpoint-data.json"
        Dropbox.Overwrite
        False
        (Just <| Date.fromTime 0)
        False
        content


storageDecoder : Decoder Storage
storageDecoder =
    decode Storage
        |> required "endpoints" (list endpointDecoder)


endpointDecoder : Decoder Endpoint
endpointDecoder =
    decode Endpoint.Endpoint
        |> required "name" Json.Decode.string
        |> required "url" Json.Decode.string
        |> required "alerts" (list Json.Decode.int)
        |> required "user" Json.Decode.string
        |> required "password" Json.Decode.string


decodeStorage : String -> Result String Storage
decodeStorage =
    Json.Decode.decodeString storageDecoder


endpointEncoder : Endpoint -> Value
endpointEncoder endpoint =
    Json.Encode.object
        [ ( "name", Json.Encode.string endpoint.name )
        , ( "url", Json.Encode.string endpoint.url )
        , ( "alerts", endpoint.alerts |> List.map (\ep -> Json.Encode.int ep) |> Json.Encode.list )
        , ( "user", Json.Encode.string endpoint.user )
        , ( "password", Json.Encode.string endpoint.password )
        ]


storageEncoder : Storage -> Value
storageEncoder storage =
    Json.Encode.object
        [ ( "endpoints"
          , storage.endpoints
                |> List.map (\ep -> endpointEncoder ep)
                |> Json.Encode.list
          )
        ]


encodeStorage : Storage -> String
encodeStorage storage =
    storage
        |> storageEncoder
        |> Json.Encode.encode 2
