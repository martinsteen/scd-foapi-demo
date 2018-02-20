module EndpointEditor exposing (Model, Msg(..), FieldContent(..), Field(..), forCreate, forModify, update, render)

import Html exposing (Html, text, div, h2, p, span)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options exposing (css, cs)
import Material.Icon as Icon
import Endpoint


type Msg msg
    = UpdateEditor ( Field, FieldContent )
    | CommitEditor Endpoint (Maybe String)
    | CancelEditor


type Field
    = Name
    | User
    | Url
    | Password


type alias ErrorInfo =
    { field : Field
    , error : String
    }


type alias Model =
    { endpoint : Endpoint
    , id : Maybe String
    , errors : List ErrorInfo
    }


type FieldContent
    = Error String
    | Value String


type alias Endpoint =
    Endpoint.Endpoint


type alias Mdl =
    Material.Model


forCreate : Endpoint -> Model
forCreate ep =
    Model ep Nothing []


forModify : Endpoint -> String -> Model
forModify ep id =
    Model ep (Just id) []


update : Model -> Field -> FieldContent -> Model
update model field fieldContent =
    case fieldContent of
        Value value ->
            { model | endpoint = updateEditor model.endpoint field value }

        Error err ->
            { model | errors = (ErrorInfo field err :: model.errors) }


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


render mdl model mdlMsg epmsg =
    let
        ( ep, id ) =
            ( model.endpoint, model.id )
    in
        Options.div [ css "margin" "10%" ]
            [ div []
                [ renderInput mdl 1 Name ep.name (findFieldError model Name) mdlMsg epmsg
                , renderInput mdl 2 Url ep.url (findFieldError model Url) mdlMsg epmsg
                ]
            , div []
                [ renderInput mdl 3 User ep.user (findFieldError model User) mdlMsg epmsg
                , renderInput mdl 4 Password ep.password (findFieldError model Password) mdlMsg epmsg
                ]
            , div []
                [ renderButton mdl ep "done" (epmsg (CommitEditor ep id)) mdlMsg
                , renderButton mdl ep "clear" (epmsg CancelEditor) mdlMsg
                ]
            ]


findFieldError : Model -> Field -> Maybe String
findFieldError model field =
    let
        ( a, b ) =
            List.partition (\aa -> aa.field == field) model.errors
    in
        List.head (List.map (\x -> x.error) a)


renderButton mdl ep icon epMsg mdlMsg =
    Button.render mdlMsg
        [ 0 ]
        mdl
        [ Button.fab
        , Button.colored
        , Options.onClick epMsg
        ]
        [ Icon.i icon ]


renderInput mdl id field value error mdlMsg epMsg =
    Textfield.render mdlMsg [ id ] mdl (fieldProperties field value epMsg error) []


fieldProperties field value epMsg maybeError =
    let
        common =
            [ Textfield.label (toString field)
            , Textfield.floatingLabel
            , Options.onInput (\value -> epMsg (UpdateEditor ( field, Value value )))
            , Textfield.value value
            ]
    in
        case maybeError of
            Nothing ->
                common

            Just err ->
                (Textfield.error err) :: common
