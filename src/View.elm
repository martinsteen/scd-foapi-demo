module View exposing (..)

import Html exposing (Html, text, div, h2, p, span)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css, cs)
import Material.Chip as Chip
import Material.Color as Color
import Material.Icon as Icon
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
    div []
        [ h2 [] [ text "SimCorp Dimension front office API demo " ]
        , loginToDropBoxButton model
        , contentView model
        , errorView model
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
        , Button.render Mdl [ 0 ] model.mdl [ Button.fab, Button.colored, Options.onClick LogInToDropbox ] [ Icon.i "add" ]
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
                    , Chip.deleteClick CancelEdit
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
                [ div [] [ renderInput mdl 1 Name ep.name, renderInput mdl 2 Url ep.url ]
                , div [] [ renderInput mdl 3 User ep.user, renderInput mdl 4 Password ep.password ]
                , div [] [ Button.render Mdl [ 0 ] mdl [ Button.fab, Button.colored, Options.onClick (SaveEndpoint ep) ] [ Icon.i "ok" ] ]
                ]

        Nothing ->
            text ""


renderInput : Mdl -> Int -> Field -> String -> Html Msg
renderInput mdl id field value =
    Textfield.render Mdl
        [ id ]
        mdl
        [ Textfield.label (toString field)
        , Textfield.floatingLabel
        , Textfield.value value
        , Options.onInput (\x -> UpdateEndpoinUnderConstruction ( field, x ))
        ]
        []
