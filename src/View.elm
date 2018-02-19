module View exposing (view)

import Html exposing (Html, map, text, div, h2, p, span)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css, cs)
import Material.Chip as Chip
import Material.Color as Color
import Material.Icon as Icon
import Msg exposing (..)
import Model exposing (..)
import EndpointEditor
import Endpoint


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


type alias Endpoint =
    Endpoint.Endpoint


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
        , Button.render Mdl [ 0 ] model.mdl [ Button.fab, Button.colored, Options.onClick StartAdd ] [ Icon.i "add" ]
        , span []
            (case model.editor of
                Nothing ->
                    []

                Just editor ->
                    [ EndpointEditor.render model.mdl editor Mdl CommitEdit CancelEdit UpdateEdit ]
            )
        ]


renderEndpointChips : List Endpoint -> Html Msg
renderEndpointChips endpoints =
    span []
        (List.map
            (\endpoint ->
                Chip.span
                    [ Options.css "margin" "5px 5px"
                    , Options.onClick (StartEdit endpoint)
                    , Chip.deleteIcon "clear"
                    , Chip.deleteClick (RemoveEndpoint endpoint)
                    ]
                    [ Chip.content []
                        [ text endpoint.name ]
                    ]
            )
            endpoints
        )
