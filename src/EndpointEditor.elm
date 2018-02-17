module EndpointEditor exposing (updateEndpointEditor, renderEndpointInput, initialModel )

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

initialModel : Endpoint -> EndpointEditorModel    
initialModel endpoint = 
    { endpoint = Nothing
    , defaultEndpoint=endpoint
    }


updateEndpointEditor : EndpointEditorMsg -> EndpointEditorModel -> ( EndpointEditorModel, Maybe Msg )
updateEndpointEditor msg model =
    case msg of
        Start endpoint ->
            case endpoint of
                Just ep ->
                    ( { model | endpoint = Just ep }, Nothing )

                Nothing ->
                    ( { model | endpoint = Just model.defaultEndpoint }, Nothing )

        Cancel ->
            ( { model | endpoint = Nothing }, Nothing )

        Commit endpoint ->
            if (endpoint.name == "") then
                ( model, Nothing )
            else
                ( model, Just (UpdateEndpoints endpoint) )

        Update ( field, value ) ->
            case model.endpoint of
                Just ep ->
                    let
                        model_ =
                            { model | endpoint = updateFieldInModelUnderConstruction ep field value }
                    in
                        ( model_, Nothing )

                Nothing ->
                    ( model, Nothing )


updateFieldInModelUnderConstruction : Endpoint -> Field -> String -> Maybe Endpoint
updateFieldInModelUnderConstruction endpoint field value =
    case field of
        Name ->
            Just { endpoint | name = value }

        Url ->
            Just { endpoint | url = value }

        Password ->
            Just { endpoint | password = value }

        User ->
            Just { endpoint | user = value }


renderEndpointInput : Mdl -> EndpointEditorModel -> Html Msg
renderEndpointInput mdl model =
    let ep = model.endpoint in  
    case ep of
        Just ep ->
            Options.div [ css "margin" "10%" ]
                [ div [] [ renderInput mdl 1 Name ep.name, renderInput mdl 2 Url ep.url ]
                , div [] [ renderInput mdl 3 User ep.user, renderInput mdl 4 Password ep.password ]
                , div [] [ renderButton mdl ep "done" (EndpointEditor (Commit ep)), renderButton mdl ep "clear" (EndpointEditor Cancel) ]
                ]

        Nothing ->
            text ""


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
