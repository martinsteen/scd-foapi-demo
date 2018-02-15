module View exposing (..)

import Html exposing (Html, text, div, h1, p, span)
import Html.Attributes exposing (class, href, style)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css, cs)
import Material.Chip as Chip
import Material.Color as Color
import Material.Textfield as Textfield
import Msg exposing (..)
import Model exposing (..)


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


type alias Endpoint =
    Model.Endpoint


type alias Mdl =
    Material.Model


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
    span []
        [ span []
            [ renderEndpointChips model.storage.endpoints ]
        , span []
            [ renderEndpointInput model.mdl model.endpointUnderConstruction ]
        ]


renderEndpointChips : List Endpoint -> Html Msg
renderEndpointChips endpoints =
    span []
        (List.map
            (\endpoint ->
                Chip.span
                    [ Options.css "margin" "5px 5px"
                    , Options.onClick (EditEndpoint endpoint)
                    , Chip.deleteIcon "cancel"
                    , Chip.deleteClick (EditEndpoint endpoint)
                    ]
                    [ Chip.content []
                        [ text endpoint.name ]
                    ]
            )
            endpoints
        )


renderEndpointInput : Mdl -> Maybe Endpoint -> Html Msg
renderEndpointInput mdl endpoint =
    case endpoint of
        Just ep ->
            Options.div [ css "margin" "10%" ]
                [ div [] [ renderInput mdl 1 "Name" ep.name ]
                , div [] [ renderInput mdl 2 "Url" ep.url ]
                , div [] [ renderInput mdl 3 "SCD User" ep.user ]
                , div [] [ renderInput mdl 4 "SCD password" ep.password ]
                ]

        Nothing ->
            text ""


renderInput : Mdl -> Int -> String -> String -> Html Msg
renderInput mdl id fieldName value =
    Textfield.render Mdl
        [ id ]
        mdl
        [ Textfield.label fieldName
        , Textfield.floatingLabel
        , Textfield.value value
        ]
        []
