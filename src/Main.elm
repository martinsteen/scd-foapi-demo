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
import EndpointEditor exposing (updateFieldInModelUnderConstruction)


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
        { endpoints = []
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

        RemoveEndpoint endpoint ->
            ( updateError model endpoint.name, Cmd.none )

        EndpointEditor (StartEndpointEditor endpoint) ->
            case endpoint of
                Just ep ->
                    ( { model | endpointUnderConstruction = Just ep }, Cmd.none )

                Nothing ->
                    ( { model | endpointUnderConstruction = Just defaultEndpoint }, Cmd.none )

        EndpointEditor CancelEndpointEditor ->
            ( { model | endpointUnderConstruction = Nothing }, Cmd.none )

        EndpointEditor (CommitEndpointEditor endpoint) ->
            if (endpoint.name == "") then
                ( model, Cmd.none )
            else
                let
                    ( model_, msg_ ) =
                        ( replaceEndpointInStorage model endpoint, updloadIfConnected model )
                in
                    ( { model_ | endpointUnderConstruction = Nothing }, msg_ )

        EndpointEditor (UpdateEndpointEditor ( field, value )) ->
            case model.endpointUnderConstruction of
                Just ep ->
                    let
                        model_ =
                            { model | endpointUnderConstruction = updateFieldInModelUnderConstruction ep field value }
                    in
                        ( model_, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updloadIfConnected : { a | auth : Maybe Dropbox.UserAuth, storage : Storage } -> Cmd Msg
updloadIfConnected model =
    case model.auth of
        Just auth ->
            uploadCmd auth model.storage

        Nothing ->
            Cmd.none


replaceEndpointInStorage : { a | storage : Storage } -> Endpoint -> { a | storage : Storage }
replaceEndpointInStorage model endpoint =
    let
        model_ =
            { model | storage = replanceEndpointInStorage model.storage endpoint }
    in
        model_


replanceEndpointInStorage : Storage -> Endpoint -> Storage
replanceEndpointInStorage storage endpoint =
    { storage | endpoints = replaceEndpointInList endpoint storage.endpoints }


replaceEndpointInList : Endpoint -> List Endpoint -> List Endpoint
replaceEndpointInList endpoint endpoints =
    let
        ( same, different ) =
            List.partition (\x -> x.name == endpoint.name) endpoints
    in
        List.sortWith compareEnpoint (endpoint :: different)


compareEnpoint : Endpoint -> Endpoint -> Order
compareEnpoint ep1 ep2 =
    compare ep1.name ep2.name


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


updateError : Model -> c -> Model
updateError model err =
    { model | error = Just (toString err) }
