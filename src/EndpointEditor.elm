module EndpointEditor exposing (Model, forCreate, forModify, update, render)

import Html exposing (Html, text, div, h2, p, span)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options exposing (css, cs)
import Material.Icon as Icon
import Endpoint


type alias Field =
    Endpoint.Field

type alias FieldInput =
    Endpoint.FieldInput

type alias Error_ =
    { field : Field
    , error : String
    }


type alias Model =
    { endpoint : Endpoint
    , id : Maybe String
    , errors : List Error_
    }


type alias Endpoint =
    Endpoint.Endpoint


type alias Mdl =
    Material.Model


forCreate : Endpoint -> Model
forCreate ep =
    Model ep Nothing []


forModify : Endpoint -> String -> Model
forModify ep id =
    Model ep (Just id) []


update : Model -> Field -> FieldInput -> Model
update model field fieldInput =
    case fieldInput of
        Endpoint.Value value ->
            { model | endpoint = updateEditor model.endpoint field value }

        Endpoint.Error err ->
            { model | errors = (Error_ field err :: model.errors) }


updateEditor : Endpoint -> Field -> String -> Endpoint
updateEditor endpoint field value =
    case field of
        Endpoint.Name ->
            { endpoint | name = value }

        Endpoint.Url ->
            { endpoint | url = value }

        Endpoint.Password ->
            { endpoint | password = value }

        Endpoint.User ->
            { endpoint | user = value }


render mdl model mdlMsg commitMsg cancelMsg updateMsg =
    let
        ( ep, id ) =
            ( model.endpoint, model.id )
    in
        Options.div [ css "margin" "10%" ]
            [ div []
                [ renderInput mdl 1 Endpoint.Name ep.name (findFieldError model Endpoint.Name) mdlMsg updateMsg
                , renderInput mdl 2 Endpoint.Url ep.url (findFieldError model Endpoint.Url) mdlMsg updateMsg
                ]
            , div []
                [ renderInput mdl 3 Endpoint.User ep.user (findFieldError model Endpoint.User) mdlMsg updateMsg
                , renderInput mdl 4 Endpoint.Password ep.password (findFieldError model Endpoint.Password) mdlMsg updateMsg
                ]
            , div [] [ renderButton mdl ep "done" (commitMsg ep id) mdlMsg, renderButton mdl ep "clear" cancelMsg mdlMsg ]
            ]


findFieldError : Model -> Field -> Maybe String
findFieldError model field =
    let
        ( a, b ) =
            List.partition (\aa -> aa.field == field) model.errors
    in
        List.head (List.map (\x -> x.error) a)


renderButton mdl ep icon onClick mdlMsg =
    Button.render mdlMsg [ 0 ] mdl [ Button.fab, Button.colored, Options.onClick onClick ] [ Icon.i icon ]


renderInput mdl id field value error mdlMsg updateMsg =
    case error of
        Nothing ->
            Textfield.render mdlMsg
                [ id ]
                mdl
                [ Textfield.label (toString field)
                , Textfield.floatingLabel
                , Textfield.value value
                , Options.onInput (\v -> updateMsg (field, Endpoint.Value value))
                ]
                []

        Just err ->
            Textfield.render mdlMsg
                [ id ]
                mdl
                [ Textfield.label (toString field)
                , Textfield.floatingLabel
                , Textfield.value value
                , Textfield.error err
                , Options.onInput (\v -> updateMsg (field, Endpoint.Value value))
                ]
                []
