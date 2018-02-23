module View exposing (view)

import Html exposing (Html, map, text, div, h1, h2, p, span, hr)
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
            { header = [ h1 [ style [ ( "padding", "3rem" ) ] ] [ text "SimCorp Dimension front office API demo" ] ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewLayout model ]
            }



viewLayout : Model -> Html Msg
viewLayout model =
    Options.div
        [ Options.center
        , css "margin-left" "20%"
        , css "margin-right" "20%"
        ]
        [ h2 [] []
        , Options.div [] [ renderIntroduction model ]
        , Options.div [] [ errorView model ]
        ]


renderIntroduction : Model -> Html Msg
renderIntroduction model =
    Html.p []
        [ h2 [] [ text "Getting Started" ]
        , Html.p []
            [ text """
            This small web page demonstrates the used of the SimCorp Dimension front office web api.
            With this aplication you will be able to create data incidents alerts."""
            ]
        , Html.p []
            [ text """
            In order to use this, you need a SimCord dimension installation that is configured to expose
            API endpoints. You have configure this application with the endpoint details to get started.
            """
            ]
        ,  renderEndpointParagraph model
        ,  maybeRenderEndpointEditor model
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
                    , text " or you can modify the endpoints by clicking on them."
                    ]
                  else
                    [ text "Currently there are no endpoints defined. You can add one here"
                    , renderButton model "add" StartAdd
                    ]
                , if (hasDropBox) then
                    [ text "If you leave this page, your changes will automatically be saved in dropbox."
                    ]
                  else
                    [ text """If you leave this page, you will have to add the
                    endpoint the next time you use it.
                    The application can store your data in dropBox, if
                    you want that, go ahead and log in to dropbox here"""
                    , renderButton model "login" LogInToDropbox
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
    let
        html =
            case model.editor of
                Nothing ->
                    text ""

                Just editor ->
                    Options.div
                        [ Elevation.e6
                        , Options.center
                        , css "height" "296px"
                        , css "width" "80%"
                        , Color.background (Color.color Color.BlueGrey Color.S50)
                        ]
                        [ EndpointEditor.render model.mdl editor Mdl EpEdit ]
    in
        div [] [ html ]


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

