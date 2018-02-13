module View exposing (..)

import Html exposing (Html, text, div, h1, p, span)
import Html.Attributes exposing (class, href, style)
import Material.Scheme
import Material.Button as Button exposing (..)
import Material.Options as Options exposing (css)
import Material.Chip as Chip
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Elevation as Elevation
import Msg exposing (..)
import Model exposing (..)
import Storage exposing (..)


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


type alias Endpoint =
    Storage.Endpoint


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
    Options.div
        [ Elevation.e4
        , css "float" "left"
        , css "margin" "10%"
        ]
        [ Options.div [ Elevation.e0, css "margin" "10%" ] [ text "Endpoints" ]
        , Options.div [ Elevation.e0, css "margin" "10%" ]
            [ renderEndpointChips model.storage.endpoints ]
        , renderEndpointInput model (List.head model.storage.endpoints)
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


renderEndpointInput : Model -> Maybe Endpoint -> Html Msg
renderEndpointInput model endpoint =
    case endpoint of
        Just ep ->
            Options.div [ css "margin" "10%" ]
                [ div [] [ renderInput model 1 "Name" ep.name ]
                , div [] [ renderInput model 2 "Url" ep.url ]
                , div [] [ renderInput model 3 "SCD ___User" ep.user ]
                , div [] [ renderInput model 4 "SCD password" ep.password ]
                ]

        Nothing ->
            Options.div [ css "margin" "10%" ]
                [ div [] [ renderInput model 1 "Name" "" ]
                , div [] [ renderInput model 2 "Url" "" ]
                , div [] [ renderInput model 3 "SCD User" "" ]
                , div [] [ renderInput model 4 "SCD password" "" ]
                ]


renderInput : Model -> Int -> String -> String -> Html Msg
renderInput model id fieldName value =
    Textfield.render Mdl
        [ id ]
        model.mdl
        [ Textfield.label fieldName
        , Textfield.floatingLabel
        , Textfield.value value
        ]
        []
