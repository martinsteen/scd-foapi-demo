module View exposing (view)

import Html exposing (Html, map, text, div, h1, h2, h4, p, span, hr)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Options as Options exposing (css, cs)
import Material.Chip as Chip
import Material.Color as Color
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Elevation as Elevation
import Msg exposing (..)
import Model exposing (..)
import EndpointEditor
import Endpoint
import Material.Grid exposing (grid, cell, size, Device(..), offset)


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


type alias Endpoint =
    Endpoint.Endpoint


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header = [ h2 [ style [ ( "padding", "1rem" ) ] ] [ text "SimCorp Dimension front office API demo" ] ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewLayout model ]
            }


viewLayout : Model -> Html Msg
viewLayout model =
    grid []
        [ cell [ offset All 1, size All 10 ]
            [ renderIntroduction model
            ]
        , cell [ offset All 1, size All 10 ]
            [ renderAlerts model
            ]
        , cell [ offset All 1, size All 10 ]
            [ errorView model
            ]
        ]


renderAlerts : Model -> Html Msg
renderAlerts model =
    if (List.isEmpty model.storage.endpoints) then
        text ""
    else
        Html.p []
            [ h2 [] [ text "Working with data incident alerts" ]
            , hr [] []
            , Html.p []
                [ text """
                In this section you will be able to issue data incident alerts aginst the system running behind the
                endpoint you defined in previos sections."""
                ]
            ]


renderIntroduction : Model -> Html Msg
renderIntroduction model =
    Html.p []
        [ h2 [] [ text "Getting Started" ]
        , hr [] []
        , Html.p []
            [ text """
            This small web page is an example of an application that uses the SimCorp Dimension front office web api. With this
            aplication you will be able to create data incidents alerts. To get started you need a running installation that is
            configured to expose these API endpoints and you need to configure this application with the details of these.
            """
            ]
        , renderEndpointParagraph model
        , grid []
            [ cell [ offset All 1, size All 10 ]
                [ maybeRenderEndpointEditor model
                ]
            ]
        ]


renderEndpointParagraph : Model -> Html Msg
renderEndpointParagraph model =
    let
        ( html, html2 ) =
            let
                hasEndpoint =
                    not (List.isEmpty model.storage.endpoints)

                hasDropBox =
                    (case model.auth of
                        Just auth ->
                            True

                        Nothing ->
                            False
                    )
            in
                ( if (hasEndpoint) then
                    [ text "You have the following endpoints defined"
                    , renderConfiguredEndpoints model
                    , text "and you can add more here "
                    , renderButton model "add" StartAdd
                    , text " or you can modify the endpoints by clicking on them. "
                    ]
                  else
                    [ text "Currently there are no endpoints defined. You can add one here. "
                    , renderButton model "add" StartAdd
                    , text " "
                    ]
                , if (hasDropBox) then
                    [ text "If you leave this page, your changes will automatically be saved in dropbox."
                    ]
                  else
                    [ text """If you leave this page, you will have to add the
                    endpoint the next time you use it.
                    The application can store your data in dropBox, if
                    you want that, go ahead and log in to dropbox here"""
                    , renderButton model "cached" LogInToDropbox
                    ]
                )
    in
        Html.p [] (html ++ html2)


renderConfiguredEndpoints : Model -> Html Msg
renderConfiguredEndpoints model =
    span []
        [ renderEndpointChips model.storage.endpoints
        ]


maybeRenderEndpointEditor : Model -> Html Msg
maybeRenderEndpointEditor model =
    case model.editor of
        Nothing ->
            text ""

        Just editor ->
            Options.div
                [ Elevation.e6
                , Options.center
                , css "padding" "10px"
                , Color.background (Color.color Color.BlueGrey Color.S50)
                ]
                [ EndpointEditor.render model.mdl editor Mdl EpEdit ]


renderButton : Model -> String -> Msg -> Html Msg
renderButton model text event =
    Button.render Mdl [ 0 ] model.mdl [ Button.minifab, Button.colored, Options.onClick event ] [ Icon.i text ]


renderEndpointChips : List Endpoint -> Html Msg
renderEndpointChips endpoints =
    span []
        (List.map
            (\endpoint ->
                Chip.span
                    [ Options.css "margin" "0px 5px"
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


errorView : Model -> Html Msg
errorView model =
    case model.error of
        Just err ->
            Html.code [] [ text ("Error --> " ++ err) ]

        Nothing ->
            text ""
