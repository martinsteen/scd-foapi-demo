module EndpointEditor exposing (updateEndpointEditor, renderEndpointInput)

import Html exposing (Html, text, div, h2, p, span)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options exposing (css, cs)
import Material.Icon as Icon
import Model exposing (..)
import Msg exposing (..)
import Dropbox
import Storage exposing (..)


type alias Msg =
    Msg.Msg


type alias Model =
    Model.Model


type alias Endpoint =
    Model.Endpoint


type alias Mdl =
    Material.Model


updateEndpointEditor : EndpointEditorMsg -> Model -> ( Model, Cmd Msg )
updateEndpointEditor msg model =
    case msg of
        Start endpoint ->
            case endpoint of
                Just ep ->
                    ( { model | endpointUnderConstruction = Just ep }, Cmd.none )

                Nothing ->
                    ( { model | endpointUnderConstruction = Just model.defaultEndpoint }, Cmd.none )

        Cancel ->
            ( { model | endpointUnderConstruction = Nothing }, Cmd.none )

        Commit endpoint ->
            if (endpoint.name == "") then
                ( model, Cmd.none )
            else
                let
                    ( model_, msg_ ) =
                        ( replaceEndpointInStorage model endpoint, updloadIfConnected model )
                in
                    ( { model_ | endpointUnderConstruction = Nothing }, msg_ )

        Update ( field, value ) ->
            case model.endpointUnderConstruction of
                Just ep ->
                    let
                        model_ =
                            { model | endpointUnderConstruction = updateFieldInModelUnderConstruction ep field value }
                    in
                        ( model_, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updloadIfConnected : { a | auth : Maybe Dropbox.UserAuth, storage : Storage } -> Cmd Msg
updloadIfConnected model =
    case model.auth of
        Just auth ->
            uploadCmd auth model.storage

        Nothing ->
            Cmd.none


replaceEndpointInStorage : { a | storage : Storage } -> Endpoint -> { a | storage : Storage }
replaceEndpointInStorage model endpoint =
    let
        model_ =
            { model | storage = replanceEndpointInStorage model.storage endpoint }
    in
        model_


replanceEndpointInStorage : Storage -> Endpoint -> Storage
replanceEndpointInStorage storage endpoint =
    { storage | endpoints = replaceEndpointInList endpoint storage.endpoints }


replaceEndpointInList : Endpoint -> List Endpoint -> List Endpoint
replaceEndpointInList endpoint endpoints =
    let
        ( same, different ) =
            List.partition (\x -> x.name == endpoint.name) endpoints
    in
        List.sortWith compareEnpoint (endpoint :: different)


compareEnpoint : Endpoint -> Endpoint -> Order
compareEnpoint ep1 ep2 =
    compare ep1.name ep2.name


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


renderEndpointInput : Mdl -> Maybe Endpoint -> Html Msg
renderEndpointInput mdl endpoint =
    case endpoint of
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
