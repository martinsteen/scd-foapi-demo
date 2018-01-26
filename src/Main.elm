module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Dropbox exposing (..)
import Dropbox.DownloadResponse exposing (..)
import Task


type Msg
    = LogInToDropbox
    | AuthResponse Dropbox.AuthorizeResult
    | FetchFileResponse (Result Dropbox.DownloadError Dropbox.DownloadResponse)


type alias Model =
    { location : Navigation.Location
    , dropboxAuth : Maybe Dropbox.UserAuth
    , fileContent : String
    }


initialModel : Navigation.Location -> Model
initialModel location =
    { location = location
    , dropboxAuth = Nothing
    , fileContent = ""
    }


view : Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Simcorp Dimension front office API demo " ]
        , p []
            [ text "Alerts"
            ]
        , Html.button
            [ Html.Events.onClick LogInToDropbox ]
            [ Html.text "Log in with Dropbox" ]
        , p []
            [ text model.fileContent ]
        ]


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
                { path = "endpoint-data.json" }
                |> Task.attempt FetchFileResponse
            )

        AuthResponse (Dropbox.DropboxAuthorizeErr _) ->
            ( model, Cmd.none )

        AuthResponse (Dropbox.UnknownAccessTokenErr _) ->
            ( model, Cmd.none )

        FetchFileResponse resp ->
            case resp of
                Ok content ->
                    ( { model | fileContent = content.content }, Cmd.none )

                Err err ->
                    ( { model | fileContent = err }, Cmd.none )


main : Program Never Model (Dropbox.Msg Msg)
main =
    Dropbox.program
        { init = \location -> ( initialModel location, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onAuth = AuthResponse
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []
