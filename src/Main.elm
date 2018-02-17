module Main exposing (..)

import Material
import Navigation
import Dropbox
import Model exposing (..)
import Msg exposing (..)
import View exposing (..)
import Storage exposing (..)
import EndpointEditor exposing (createEditor, updateEndpointEditor)


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
        { endpoints = [] }
    , mdl = Material.model
    , editor = Nothing
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

        UpdateEndpoints endpoint ->
            let model_ = { model | storage = replanceEndpointInStorage model.storage endpoint, editor = Nothing } 
            in ( model_, updloadIfConnected model_ )

        CancelEdit ->
            ( { model | editor = Nothing }, Cmd.none )

        StartAdd ->
            ( { model | editor = Just (createEditor defaultEndpoint) }, Cmd.none )

        StartEdit endpoint ->
            ( { model | editor = Just (createEditor endpoint) }, Cmd.none )

        EndpointEditor editorMessage ->
            case model.editor of
                Just editor ->
                    case (updateEndpointEditor editorMessage editor) of
                        ( editorModel , Just msg ) ->
                            update msg { model | editor = Just editorModel } 

                        ( editorModel, Nothing ) ->
                            ( { model | editor = Just editorModel }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )


updloadIfConnected : Model -> Cmd Msg
updloadIfConnected model =
    case model.auth of
        Just auth ->
            uploadCmd auth model.storage

        Nothing ->
            Cmd.none


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


updateAuth : { b | auth : a } -> { d | userAuth : c } -> { b | auth : Maybe c }
updateAuth model authentication =
    { model | auth = Just authentication.userAuth }


updateError : Model -> c -> Model
updateError model err =
    { model | error = Just (toString err) }
