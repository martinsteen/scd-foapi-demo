module EndpointEditor exposing (forCreate, forModify, update, render )

import Html exposing (Html, text, div, h2, p, span)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options exposing (css, cs)
import Material.Icon as Icon
import Model exposing (..)
import Msg exposing (..)


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


type alias Endpoint =
    Model.Endpoint

type alias Mdl =
    Material.Model

forCreate : Endpoint -> EndpointEditorModel 
forCreate ep = 
    EndpointEditorModel ep Nothing

forModify : Endpoint -> String -> EndpointEditorModel 
forModify ep id = 
    EndpointEditorModel ep (Just id)


update : EndpointEditorMsg -> EndpointEditorModel -> EndpointEditorModel
update msg model =
    case msg of
        Update ( field, value ) ->
            { model | endpoint = updateEditor model.endpoint field value }


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


render : Mdl -> EndpointEditorModel -> Html Msg
render mdl model =
    let (ep,id) = (model.endpoint, model.id) in  
        Options.div [ css "margin" "10%" ]
            [ div [] [ renderInput mdl 1 Name ep.name, renderInput mdl 2 Url ep.url ]
            , div [] [ renderInput mdl 3 User ep.user, renderInput mdl 4 Password ep.password ]
            , div [] [ renderButton mdl ep "done" (CommitEdit ep id), renderButton mdl ep "clear" CancelEdit ]
            ]


renderButton : Mdl -> Endpoint -> String -> Msg -> Html Msg
renderButton mdl ep icon onClick =
    Button.render Mdl [ 0 ] mdl [ Button.fab, Button.colored, Options.onClick onClick ] [ Icon.i icon ]


renderInput : Mdl -> Int -> Field -> String -> Html Msg
renderInput mdl id field value =
    Textfield.render Mdl
        [ id ]
        mdl
        [ Textfield.label (toString field)
        , Textfield.floatingLabel
        , Textfield.value value
        , Options.onInput (\value -> EndpointEditor (Update ( field, value )))
        ]
        []
