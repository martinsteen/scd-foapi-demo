module EndpointEditor exposing (Model, forCreate, forModify, update, render)

import Html exposing (Html, text, div, h2, p, span)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options exposing (css, cs)
import Material.Icon as Icon
import Endpoint
import Msg exposing (..)


type alias Error =
    { field : Field
    , error : String
    }


type alias Model =
    { endpoint : Endpoint
    , id : Maybe String
    , errors : List Error
    }


type alias Msg =
    Msg.Msg


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
        Value value ->
            { model | endpoint = updateEditor model.endpoint field value }

        Error err ->
            { model | errors = (Error field err :: model.errors) }


updateEditor : Endpoint -> Field -> String -> Endpoint
updateEditor endpoint field value =
    case field of
        Name ->
            { endpoint | name = value }

        Url ->
            { endpoint | url = value }

        Password ->
            { endpoint | password = value }

        User ->
            { endpoint | user = value }


render : Mdl -> Model -> Html Msg
render mdl model =
    let
        ( ep, id ) =
            ( model.endpoint, model.id )
    in
        Options.div [ css "margin" "10%" ]
            [ div []
                [ renderInput mdl 1 Name ep.name (findFieldError model Name)
                , renderInput mdl 2 Url ep.url (findFieldError model Url)
                ]
            , div []
                [ renderInput mdl 3 User ep.user (findFieldError model User)
                , renderInput mdl 4 Password ep.password (findFieldError model Password)
                ]
            , div [] [ renderButton mdl ep "done" (CommitEdit ep id), renderButton mdl ep "clear" CancelEdit ]
            ]


findFieldError : Model -> Field -> Maybe String
findFieldError model field =
    let 
        ( a,b ) = 
            List.partition (\aa -> aa.field == field ) model.errors
    in
        List.head (List.map (\x -> x.error) a)


renderButton : Mdl -> Endpoint -> String -> Msg -> Html Msg
renderButton mdl ep icon onClick =
    Button.render Mdl [ 0 ] mdl [ Button.fab, Button.colored, Options.onClick onClick ] [ Icon.i icon ]


renderInput : Mdl -> Int -> Field -> String -> Maybe String -> Html Msg
renderInput mdl id field value error =
    case error of
        Nothing ->
            Textfield.render Mdl
                [ id ]
                mdl
                [ Textfield.label (toString field)
                , Textfield.floatingLabel
                , Textfield.value value
                , Options.onInput (\value -> (UpdateEdit ( field, Value value )))
                ] 
                []
        Just err ->
            Textfield.render Mdl
                [ id ]
                mdl
                [ Textfield.label (toString field)
                , Textfield.floatingLabel
                , Textfield.value value
                , Textfield.error err
                , Options.onInput (\value -> (UpdateEdit ( field, Value value )))
                ] 
                []
