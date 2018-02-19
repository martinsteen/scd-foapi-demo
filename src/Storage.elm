module Storage exposing (Storage, findUpdateProblems, update, decode, uploadTask, downloadTask)

import Json.Encode exposing (Value, object, string)
import Json.Decode exposing (int, string, float, Decoder, list)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Dropbox
import Task
import Date
import Endpoint


type alias Endpoint =
    Endpoint.Endpoint


type alias Storage =
    { endpoints : List Endpoint }


downloadTask : Dropbox.UserAuth -> Task.Task Dropbox.DownloadError Dropbox.DownloadResponse
downloadTask auth =
    Dropbox.download auth
        { path = "/endpoint-data.json" }


uploadTask : Dropbox.UserAuth -> Storage -> Task.Task Dropbox.UploadError Dropbox.UploadResponse
uploadTask auth storage =
    storage
        |> encode
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
    Json.Decode.Pipeline.decode Storage
        |> required "endpoints" (list endpointDecoder)


endpointDecoder : Decoder Endpoint
endpointDecoder =
    Json.Decode.Pipeline.decode Endpoint.Endpoint
        |> required "name" Json.Decode.string
        |> required "url" Json.Decode.string
        |> required "alerts" (list Json.Decode.int)
        |> required "user" Json.Decode.string
        |> required "password" Json.Decode.string


decode : String -> Result String Storage
decode =
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


encode : Storage -> String
encode storage =
    storage
        |> storageEncoder
        |> Json.Encode.encode 2


update : Storage -> Endpoint -> Maybe String -> Storage
update storage endpoint oldName =
    case oldName of
        Just oldName ->
            { storage | endpoints = replaceEndpoint endpoint oldName storage.endpoints }

        Nothing ->
            { storage | endpoints = addEndpoint endpoint storage.endpoints }


replaceEndpoint : Endpoint -> String -> List Endpoint -> List Endpoint
replaceEndpoint endpoint oldName endpoints =
    let
        ( same, different ) =
            List.partition (\x -> x.name == oldName) endpoints
    in
        List.sortWith compareEnpoint (endpoint :: different)


addEndpoint : Endpoint -> List Endpoint -> List Endpoint
addEndpoint endpoint endpoints =
    List.sortWith compareEnpoint (endpoint :: endpoints)


compareEnpoint : Endpoint -> Endpoint -> Order
compareEnpoint ep1 ep2 =
    compare ep1.name ep2.name


containsName : Storage -> String -> Bool
containsName storage id =
    List.any (\x -> x.name == id) storage.endpoints


findUpdateProblems : Storage -> Endpoint -> Maybe String -> Maybe String
findUpdateProblems storage endpoint oldName =
    if (endpoint.name == "") then
        Just "please provide a name"
    else
        case oldName of
            Just oldName ->
                if (oldName /= endpoint.name && containsName storage endpoint.name) then
                    Just "this name already  exist"
                else
                    Nothing

            Nothing ->
                if (containsName storage endpoint.name) then
                    Just "this name already  exist"
                else
                    Nothing
