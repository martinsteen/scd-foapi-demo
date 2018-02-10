module Main exposing (..)

import Html exposing (Html, text, div, h1, p)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Navigation
import Dropbox exposing (..)
import Task
import Date exposing (Date)
import Json.Encode exposing (Value, object, string)
import Json.Decode exposing (int, string, float, Decoder, list)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type Msg
    = LogInToDropbox
    | AuthResponse Dropbox.AuthorizeResult
    | FetchFileResponse (Result Dropbox.DownloadError Dropbox.DownloadResponse)
    | PutFileReponse (Result Dropbox.UploadError Dropbox.UploadResponse)


type alias Model =
    { location : Navigation.Location
    , dropboxAuth : Maybe Dropbox.UserAuth
    , storage : Storage
    , errorMessage : String
    }


type alias Storage =
    { endpoints : List String }


initialModel : Navigation.Location -> Model
initialModel location =
    { location = location
    , dropboxAuth = Nothing
    , storage = { endpoints = [ "https://dk01ws1672.scdom.net:44320/odata", "https://dk01wv2028.scdom.net:44320/odata" ] }
    , errorMessage = ""
    }


view : Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Simcorp Dimension front office API demo " ]
        , p []
            [ text "Alerts" ]
        , Html.button
            [ Html.Events.onClick LogInToDropbox ]
            [ text "Log in with Dropbox" ]
        , p []
            [ contentView model
            , errorView model
            ]
        ]


errorView : Model -> Html Msg
errorView model =
    if String.isEmpty model.errorMessage then
        text ""
    else
        Html.code [] [ text ("Error --> " ++ model.errorMessage) ]


contentView : Model -> Html Msg
contentView model =
    if List.isEmpty model.storage.endpoints then
        text ""
    else
        div [] <| text ("available endpoints") :: (List.map (\endpoint -> p [] [ Html.code [] [ text (endpoint) ] ]) model.storage.endpoints)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogInToDropbox ->
            ( model, authorizeCmd model.location )

        AuthResponse (Dropbox.AuthorizeOk auth) ->
            ( { model | dropboxAuth = Just auth.userAuth }, downloadCmd auth.userAuth )

        AuthResponse (Dropbox.DropboxAuthorizeErr err) ->
            ( { model | errorMessage = toString err }, Cmd.none )

        AuthResponse (Dropbox.UnknownAccessTokenErr err) ->
            ( { model | errorMessage = toString err }, Cmd.none )

        PutFileReponse resp ->
            ( model, Cmd.none )

        FetchFileResponse resp ->
            case resp of
                Ok content ->
                    case decodeStorage content.content of
                        Ok endpoints ->
                            ( { model | storage = endpoints }, Cmd.none )

                        Err err ->
                            ( { model | errorMessage = toString err }, Cmd.none )

                Err err ->
                    case err of
                        PathDownloadError dlerr ->
                            case model.dropboxAuth of
                                Just auth ->
                                    ( model, uploadCmd auth model.storage )

                                Nothing ->
                                    ( { model | errorMessage = toString err }, Cmd.none )

                        _ ->
                            ( { model | errorMessage = toString err }, Cmd.none )


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
        |> required "endpoints" (list Json.Decode.string)


decodeStorage : String -> Result String Storage
decodeStorage =
    Json.Decode.decodeString storageDecoder


storageEncoder : Storage -> Value
storageEncoder storage =
    Json.Encode.object
        [ ( "endpoints"
          , storage.endpoints
                |> List.map (\ep -> Json.Encode.string ep)
                |> Json.Encode.list
          )
        ]


encodeStorage : Storage -> String
encodeStorage storage =
    storage
        |> storageEncoder
        |> Json.Encode.encode 2
