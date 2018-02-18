module Main exposing (..)

import Material
import Navigation
import Dropbox
import Model exposing (..)
import Msg exposing (..)
import View exposing (..)
import Storage exposing (..)
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
            ( updateAuth model auth, downloadCmd auth.userAuth |> Task.attempt FetchFileResponse )

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
                    ( model, uploadCmd auth model.storage |> Task.attempt PutFileReponse )

                Nothing ->
                    ( updateError model err, Cmd.none )

        FetchFileResponse (Err err) ->
            ( updateError model err, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        RemoveEndpoint endpoint ->
            ( updateError model endpoint.name, Cmd.none )

        CommitEdit endpoint id ->
            case (searchErrors model endpoint id) of
                Just err ->
                    ( updateEditor model Name (Error err), Cmd.none )

                Nothing ->
                    saveEndpoint model endpoint id

        CancelEdit ->
            ( { model | editor = Nothing }, Cmd.none )

        StartAdd ->
            ( { model | editor = Just (EndpointEditor.forCreate defaultEndpoint) }, Cmd.none )

        StartEdit endpoint ->
            ( { model | editor = Just (EndpointEditor.forModify endpoint endpoint.name) }, Cmd.none )

        UpdateEdit ( field, fieldInput ) ->
            ( updateEditor model field fieldInput, Cmd.none )


updateEditor : Model -> Field -> FieldInput -> Model
updateEditor model field fieldInput =
    case model.editor of
        Just editor ->
            { model | editor = Just (EndpointEditor.update editor field fieldInput) }

        Nothing ->
            model


searchErrors : Model -> Endpoint -> Maybe String -> Maybe String
searchErrors model endpoint id =
    if (endpoint.name == "") then
        Just "please provide a name"
    else 
        case id of
            Just id ->
                if (id /= endpoint.name && containsName model endpoint.name) then
                    Just "this name already  exist"
                else
                    Nothing

            Nothing ->
                if (containsName model endpoint.name) then
                    Just "this name already  exist"
                else
                    Nothing


saveEndpoint : Model -> Endpoint -> Maybe String -> ( Model, Cmd Msg )
saveEndpoint model endpoint id =
    let
        model_ =
            { model | storage = updateStorage model.storage endpoint id, editor = Nothing }
    in
        ( model_, updloadIfConnected model_ )


containsName : Model -> String -> Bool
containsName model id =
    List.any (\x -> x.name == id) model.storage.endpoints


updloadIfConnected : Model -> Cmd Msg
updloadIfConnected model =
    case model.auth of
        Just auth ->
            uploadCmd auth model.storage |> Task.attempt PutFileReponse

        Nothing ->
            Cmd.none


updateStorage : Storage -> Endpoint -> Maybe String -> Storage
updateStorage storage endpoint id =
    case id of
        Just id ->
            { storage | endpoints = replaceEndpointInList endpoint id storage.endpoints }

        Nothing ->
            { storage | endpoints = addEndpointInList endpoint storage.endpoints }


replaceEndpointInList : Endpoint -> String -> List Endpoint -> List Endpoint
replaceEndpointInList endpoint id endpoints =
    let
        ( same, different ) =
            List.partition (\x -> x.name == id) endpoints
    in
        List.sortWith compareEnpoint (endpoint :: different)


addEndpointInList : Endpoint -> List Endpoint -> List Endpoint
addEndpointInList endpoint endpoints =
    List.sortWith compareEnpoint (endpoint :: endpoints)


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
