module Main exposing (..)

import Material
import Navigation
import Dropbox
import Task
import Date
import Model exposing (..)
import Msg exposing (..)
import View exposing (..)
import Storage exposing (..)


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


type alias Endpoint =
    Model.Endpoint


type alias Storage =
    Model.Storage


initialModel : Navigation.Location -> Model
initialModel location =
    { auth = Nothing
    , error = Nothing
    , location = location
    , storage =
        { endpoints =
            [ { name = "6.3/TESTDK2", url = "https://dk01ws1672.scdom.net:44320/odata", alerts = [], user = "MSIEXT", password = "MSIEXT" }
            , { name = "Dev/FOART-P", url = "https://dk01ws1672.scdom.net:44320/odata", alerts = [], user = "MSIEXT", password = "MSIEXT" }
            ]
        }
    , mdl = Material.model
    , endpointUnderConstruction = Nothing
    }

defaultEndpoint : Endpoint
defaultEndpoint =
    { name = "", url = "https://xxx/odata", alerts = [], user = "", password = "" }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogInToDropbox ->
            ( model, authorizeCmd model.location )

        PutFileReponse resp ->
            ( model, Cmd.none )

        AuthResponse (Dropbox.AuthorizeOk auth) ->
            ( updateAuth model auth, downloadCmd auth.userAuth )

        AuthResponse (Dropbox.DropboxAuthorizeErr err) ->
            ( updateError model err, Cmd.none )

        AuthResponse (Dropbox.UnknownAccessTokenErr err) ->
            ( updateError model err, Cmd.none )

        FetchFileResponse (Ok content) ->
            case Storage.decodeStorage content.content of
                Ok endpoints ->
                    ( { model | storage = endpoints }, Cmd.none )

                Err decodeErr ->
                    ( updateError model decodeErr, Cmd.none )

        FetchFileResponse (Err (Dropbox.PathDownloadError err)) ->
            case model.auth of
                Just auth ->
                    ( model, uploadCmd auth model.storage )

                Nothing ->
                    ( updateError model err, Cmd.none )

        FetchFileResponse (Err err) ->
            ( updateError model err, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        StartEndpointEditor endpoint ->
            case endpoint of
                Just ep ->
                    ( { model | endpointUnderConstruction = Just ep }, Cmd.none )
                Nothing ->
                    ( { model | endpointUnderConstruction = Just defaultEndpoint }, Cmd.none )

        RemoveEndpoint endpoint ->
            ( updateError model endpoint.name, Cmd.none )

        SaveEndpoint endpoint ->
            case model.auth of
                Just auth ->
                    let model_ =
                        { model | storage = replanceEndpointInStorage model.storage endpoint }
                    in 
                        ( { model_ | endpointUnderConstruction = Nothing } , uploadCmd auth model_.storage )

                Nothing ->
                    ( updateError model "You have to login to dropbox first", Cmd.none )

        UpdateEndportEditor ( field, value ) ->
            case model.endpointUnderConstruction of
                Just ep ->
                    let model_ =
                        { model | endpointUnderConstruction = updateFieldInModelUnderConstruction ep field value }
                    in 
                        ( model_, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelEndpointEditor ->
            ( { model | endpointUnderConstruction = Nothing }, Cmd.none )


updateFieldInModelUnderConstruction : Endpoint -> Field -> String -> Maybe Endpoint
updateFieldInModelUnderConstruction endpoint field value =
    case field of
        Name ->
            Just { endpoint | name = value }

        Url ->
            Just { endpoint | url = value }

        Password ->
            Just { endpoint | password = value }

        User ->
            Just { endpoint | user = value }


replanceEndpointInStorage : Storage -> Endpoint -> Storage
replanceEndpointInStorage storage endpoint =
    { storage | endpoints = replaceEndpointInList endpoint storage.endpoints }


replaceEndpointInList : Endpoint -> List Endpoint -> List Endpoint
replaceEndpointInList endpoint endpoints =
    let
        ( same, different ) =
            List.partition (\x -> x.name == endpoint.name) endpoints
    in
        endpoint :: different


main : Program Never Model (Dropbox.Msg Msg)
main =
    Dropbox.program
        { init = \location -> ( initialModel location, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        , onAuth = AuthResponse
        }


uploadCmd : Dropbox.UserAuth -> Storage -> Cmd Msg
uploadCmd auth storage =
    storage
        |> Storage.encodeStorage
        |> createUploadReq
        |> Dropbox.upload auth
        |> Task.attempt PutFileReponse


createUploadReq : String -> Dropbox.UploadRequest
createUploadReq content =
    Dropbox.UploadRequest
        "/endpoint-data.json"
        Dropbox.Overwrite
        False
        (Just <| Date.fromTime 0)
        False
        content


authorizeCmd : Navigation.Location -> Cmd Msg
authorizeCmd location =
    Dropbox.authorize
        { clientId = "35lv4p68hbcchyn"
        , state = Nothing
        , requireRole = Nothing
        , forceReapprove = False
        , disableSignup = False
        , locale = Nothing
        , forceReauthentication = False
        }
        location


downloadCmd : Dropbox.UserAuth -> Cmd Msg.Msg
downloadCmd auth =
    Dropbox.download auth
        { path = "/endpoint-data.json" }
        |> Task.attempt FetchFileResponse


updateAuth : { b | auth : a } -> { d | userAuth : c } -> { b | auth : Maybe c }
updateAuth model authentication =
    { model | auth = Just authentication.userAuth }


updateError : { b | error : a } -> c -> { b | error : Maybe String }
updateError model err =
    { model | error = Just (toString err) }
