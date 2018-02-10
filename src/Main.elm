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


decodeStorage : String -> Result String Storage
decodeStorage =
    Json.Decode.decodeString storageDecoder


storageDecoder : Decoder Storage
storageDecoder =
    decode Storage
        |> required "endpoints" (list Json.Decode.string)


encodeStorage : Storage -> String
encodeStorage storage =
    Json.Encode.encode 2 (listEncoder storage.endpoints)


listEncoder : List String -> Value
listEncoder list =
    Json.Encode.object [ ( "endpoints", Json.Encode.list (List.map (\ep -> Json.Encode.string ep) list) ) ]


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


createRq : String -> Dropbox.UploadRequest
createRq content =
    Dropbox.UploadRequest
        "/endpoint-data.json"
        Overwrite
        False
        (Just <| Date.fromTime 0)
        False
        content


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LogInToDropbox ->
            ( model
            , Dropbox.authorize
                { clientId = "35lv4p68hbcchyn"
                , state = Nothing
                , requireRole = Nothing
                , forceReapprove = False
                , disableSignup = False
                , locale = Nothing
                , forceReauthentication = False
                }
                model.location
            )

        AuthResponse (Dropbox.AuthorizeOk auth) ->
            ( { model | dropboxAuth = Just auth.userAuth }
            , Dropbox.download auth.userAuth
                { path = "/endpoint-data.json" }
                |> Task.attempt FetchFileResponse
            )

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
                                    ( model
                                    , Dropbox.upload auth (createRq (encodeStorage model.storage))
                                        |> Task.attempt PutFileReponse
                                    )

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
