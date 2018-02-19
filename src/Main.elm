module Main exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Material
import Navigation
import Dropbox
import View
import Storage
import EndpointEditor
import Task
import Endpoint


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


type alias Endpoint =
    Endpoint.Endpoint


type alias Storage =
    Storage.Storage


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
            ( updateAuth model auth, Storage.downloadTask auth.userAuth |> Task.attempt FetchFileResponse )

        AuthResponse (Dropbox.DropboxAuthorizeErr err) ->
            ( updateError model err, Cmd.none )

        AuthResponse (Dropbox.UnknownAccessTokenErr err) ->
            ( updateError model err, Cmd.none )

        FetchFileResponse (Ok content) ->
            case Storage.decode content.content of
                Ok endpoints ->
                    ( { model | storage = endpoints }, Cmd.none )

                Err decodeErr ->
                    ( updateError model decodeErr, Cmd.none )

        FetchFileResponse (Err (Dropbox.PathDownloadError err)) ->
            case model.auth of
                Just auth ->
                    ( model, Storage.uploadTask auth model.storage |> Task.attempt PutFileReponse )

                Nothing ->
                    ( updateError model err, Cmd.none )

        FetchFileResponse (Err err) ->
            ( updateError model err, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        RemoveEndpoint endpoint ->
            ( updateError model endpoint.name, Cmd.none )

        CommitEdit endpoint oldName ->
            case (Storage.findUpdateProblems model.storage endpoint oldName) of
                Just err ->
                    ( updateEditor model EndpointEditor.Name (EndpointEditor.Error err), Cmd.none )

                Nothing ->
                    saveEndpoint model endpoint oldName

        CancelEdit ->
            ( { model | editor = Nothing }, Cmd.none )

        StartAdd ->
            ( { model | editor = Just (EndpointEditor.forCreate defaultEndpoint) }, Cmd.none )

        StartEdit endpoint ->
            ( { model | editor = Just (EndpointEditor.forModify endpoint endpoint.name) }, Cmd.none )

        UpdateEdit ( field, fieldInput ) ->
            ( updateEditor model field fieldInput, Cmd.none )


updateEditor : Model -> EndpointEditor.Field -> EndpointEditor.FieldContent -> Model
updateEditor model field fieldInput =
    case model.editor of
        Just editor ->
            { model | editor = Just (EndpointEditor.update editor field fieldInput) }

        Nothing ->
            model


saveEndpoint : Model -> Endpoint -> Maybe String -> ( Model, Cmd Msg )
saveEndpoint model endpoint oldName =
    let
        model_ =
            { model | storage = Storage.update model.storage endpoint oldName, editor = Nothing }
    in
        ( model_, updloadIfConnected model_ )


updloadIfConnected : Model -> Cmd Msg
updloadIfConnected model =
    case model.auth of
        Just auth ->
            Storage.uploadTask auth model.storage |> Task.attempt PutFileReponse

        Nothing ->
            Cmd.none


main : Program Never Model (Dropbox.Msg Msg)
main =
    Dropbox.program
        { init = \location -> ( initialModel location, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = View.view
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
