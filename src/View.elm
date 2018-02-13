module View exposing (..)

import Html exposing (Html, text, div, h1, p, span)
import Html.Attributes exposing (class, href, style)
import Material.Scheme
import Material.Button as Button exposing (..)
import Material.Options as Options exposing (css)
import Material.Chip as Chip
import Material.Card as Card
import Material.Color as Color
import Material.Icon as Icon
import Material.Card as Card
import Material.Textfield as Textfield
import Msg exposing (..)
import Model exposing (..)


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


white : Options.Property c m
white =
    Color.text Color.white


errorView : Model -> Html Msg
errorView model =
    case model.error of
        Just err ->
            Html.code [] [ text ("Error --> " ++ err) ]

        Nothing ->
            text ""


loginToDropBoxButton : Model -> Html Msg
loginToDropBoxButton model =
    case model.auth of
        Nothing ->
            Button.render Mdl
                [ 0 ]
                model.mdl
                [ Options.onClick LogInToDropbox
                ]
                [ text "Log in to Dropbox" ]

        Just auth ->
            text ""


view : Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "SimCorp Dimension front office API demo " ]
        , (loginToDropBoxButton model)
        , p []
            [ contentView model
            , errorView model
            ]
        ]
        |> Material.Scheme.top


contentView : Model -> Html Msg
contentView model =
    if List.isEmpty model.storage.endpoints then
        text ""
    else
        cardView model


cardView : Model -> Html Msg
cardView model =
    Card.view
        [ Color.background (Color.color Color.DeepOrange Color.S400)
        , css "width" "100%"
        ]
        [ Card.title [ Card.border ] [ Card.subhead [ white ] [ text "Endpoints" ] ]
        , Card.text [ white, css "text-align" "left" ]
            [ renderEndpointChips model.storage.endpoints
            , renderEndpointInput model
            ]
        , Card.actions
            [ Card.border, css "vertical-align" "center", css "text-align" "right", white ]
            [ Button.render Mdl
                [ 8, 1 ]
                model.mdl
                [ Button.icon, Button.ripple ]
                [ Icon.i "favorite_border" ]
            , Button.render Mdl
                [ 8, 2 ]
                model.mdl
                [ Button.icon, Button.ripple ]
                [ Icon.i "event_available" ]
            ]
        ]


renderEndpointChips : List Endpoint -> Html Msg
renderEndpointChips endpoints =
    span []
        (List.map
            (\endpoint ->
                Chip.span
                    [ Chip.deleteIcon "cancel", css "margin" "12px" ]
                    [ Chip.content [] [ text endpoint.name ]
                    ]
            )
            endpoints
        )


renderEndpointInput : Model -> Html Msg
renderEndpointInput model =
    div []
        [ Textfield.render Mdl
            [ 1 ]
            model.mdl
            [ Textfield.label "Name"
            , Textfield.floatingLabel
            , Textfield.text_
            ]
            []
        , Textfield.render Mdl
            [ 2 ]
            model.mdl
            [ Textfield.label "Url"
            , Textfield.floatingLabel
            , Textfield.text_
            ]
            []
        , Textfield.render Mdl
            [ 3 ]
            model.mdl
            [ Textfield.label "SCD User"
            , Textfield.floatingLabel
            , Textfield.text_
            ]
            []
        , Textfield.render Mdl
            [ 4 ]
            model.mdl
            [ Textfield.label "SCD password"
            , Textfield.floatingLabel
            ]
            []
        ]
