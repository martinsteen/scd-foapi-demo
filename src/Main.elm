module Main exposing (..)

import Html exposing (Html, text, div, h1, p)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Dropbox exposing (..)
import Task
import Json.Decode exposing (string, list, decodeString, Decoder)


type Msg
    = LogInToDropbox
    | AuthResponse Dropbox.AuthorizeResult
    | FetchFileResponse (Result Dropbox.DownloadError Dropbox.DownloadResponse)


type alias Model =
    { location : Navigation.Location
    , dropboxAuth : Maybe Dropbox.UserAuth
    , endpoints : List String
    , errorMessage : String
    }


endPointsDecoder : Decoder (List String)
endPointsDecoder =
    Json.Decode.list string


initialModel : Navigation.Location -> Model
initialModel location =
    { location = location
    , dropboxAuth = Nothing
    , endpoints = []
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
    if List.isEmpty model.endpoints then
        text ""
    else
        div [] <| text ("available endpoints") :: (List.map (\endpoint -> p [] [ Html.code [] [ text (endpoint) ] ]) model.endpoints)


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

        FetchFileResponse resp ->
            case resp of
                Ok content ->
                    case decodeString endPointsDecoder content.content of
                        Ok endpoints ->
                            ( { model | endpoints = endpoints }, Cmd.none )

                        Err err ->
                            ( { model | errorMessage = toString err }, Cmd.none )

                Err err ->
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
