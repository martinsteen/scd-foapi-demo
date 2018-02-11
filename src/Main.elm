module Main exposing (..)

import Html exposing (Html, text, div, h1, p)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (..)
import Navigation
import Dropbox exposing (..)
import Task
import Date exposing (Date)
import Json.Encode exposing (Value, object, string)
import Json.Decode exposing (int, string, float, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Chip as Chip


type Msg
    = LogInToDropbox
    | AuthResponse Dropbox.AuthorizeResult
    | FetchFileResponse (Result Dropbox.DownloadError Dropbox.DownloadResponse)
    | PutFileReponse (Result Dropbox.UploadError Dropbox.UploadResponse)
    | Mdl (Material.Msg Msg)


type alias Model =
    { storage : Storage
    , location : Navigation.Location
    , auth : Maybe Dropbox.UserAuth
    , error : Maybe String
    , mdl : Material.Model
    }


type alias Endpoint =
    { name : String
    , alerts : List Int
    }


type alias Storage =
    { endpoints : List Endpoint }


initialModel : Navigation.Location -> Model
initialModel location =
    { auth = Nothing
    , error = Nothing
    , location = location
    , storage = { endpoints = [] }
    , mdl = Material.model
    }


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Simcorp Dimension front office API demo " ]
        , (loginToDropBoxButton model)
        , p []
            [ contentView model
            , errorView model
            ]
        ]
        |> Material.Scheme.top


errorView : Model -> Html Msg
errorView model =
    case model.error of
        Just err ->
            Html.code [] [ text ("Error --> " ++ err) ]

        Nothing ->
            Html.div [] []


loginToDropBoxButton : Model -> Html Msg
loginToDropBoxButton model =
    case model.auth of
        Nothing ->
            Button.render Mdl
                [ 0 ]
                model.mdl
                [ Options.onClick LogInToDropbox
                , css "margin" "0 24px"
                ]
                [ text "Log in with Dropbox" ]

        Just auth ->
            text ""


contentView : Model -> Html Msg
contentView model =
    if List.isEmpty model.storage.endpoints then
        text ""
    else
        div []
            (List.map
                (\endpoint ->
                    div []
                        [ Chip.span
                            [ Chip.deleteIcon "cancel" ]
                            [ Chip.content []
                                [ text endpoint.name ]
                            ]
                        , Html.br [] []
                        ]
                )
                model.storage.endpoints
            )


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
            case decodeStorage content.content of
                Ok endpoints ->
                    ( { model | storage = endpoints }, Cmd.none )

                Err decodeErr ->
                    ( updateError model decodeErr, Cmd.none )

        FetchFileResponse (Err (PathDownloadError err)) ->
            case model.auth of
                Just auth ->
                    ( model, uploadCmd auth model.storage )

                Nothing ->
                    ( updateError model err, Cmd.none )

        FetchFileResponse (Err err) ->
            ( updateError model err, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


main : Program Never Model (Dropbox.Msg Msg)
main =
    Dropbox.program
        { init = \location -> ( initialModel location, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        , onAuth = AuthResponse
        }


uploadCmd : UserAuth -> Storage -> Cmd Msg
uploadCmd auth storage =
    storage
        |> encodeStorage
        |> createUploadReq
        |> Dropbox.upload auth
        |> Task.attempt PutFileReponse


createUploadReq : String -> Dropbox.UploadRequest
createUploadReq content =
    Dropbox.UploadRequest
        "/endpoint-data.json"
        Overwrite
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


downloadCmd : Dropbox.UserAuth -> Cmd Msg
downloadCmd auth =
    Dropbox.download auth
        { path = "/endpoint-data.json" }
        |> Task.attempt FetchFileResponse


storageDecoder : Decoder Storage
storageDecoder =
    decode Storage
        |> required "endpoints" (list endpointDecoder)


endpointDecoder : Decoder Endpoint
endpointDecoder =
    decode Endpoint
        |> required "name" Json.Decode.string
        |> required "alerts" (list Json.Decode.int)


decodeStorage : String -> Result String Storage
decodeStorage =
    Json.Decode.decodeString storageDecoder


endpointEncoder : Endpoint -> Value
endpointEncoder endpoint =
    Json.Encode.object
        [ ( "name", Json.Encode.string endpoint.name )
        , ( "alerts", endpoint.alerts |> List.map (\ep -> Json.Encode.int ep) |> Json.Encode.list )
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


updateAuth : { b | auth : a } -> { d | userAuth : c } -> { b | auth : Maybe c }
updateAuth model authentication =
    { model | auth = Just authentication.userAuth }


updateError : { b | error : a } -> c -> { b | error : Maybe String }
updateError model err =
    { model | error = Just (toString err) }
