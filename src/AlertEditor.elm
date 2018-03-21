module AlertEditor exposing (Model, Msg(..), FieldContent(..), Field(..), update)

import Alert
import Html exposing (Html, text, div, h2, p, span)
import Html.Attributes exposing (style)
import Material
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options as Options exposing (css, cs)
import Material.Icon as Icon
import Material.Grid exposing (grid, cell, size, Device(..), offset)


type Msg msg
    = UpdateEditor ( Field, FieldContent )
    | CommitEditor Alert (Maybe String)
    | CancelEditor


type Field
    = Id
    | Note


type alias ErrorInfo =
    { field : Field
    , error : String
    }


type alias Model =
    { alert : Alert
    , errors : List ErrorInfo
    }


type FieldContent
    = Error String
    | Value String


type alias Alert =
    Alert.Alert


type alias Mdl =
    Material.Model


update : Model -> Field -> FieldContent -> Model
update model field fieldContent =
    case fieldContent of
        Value value ->
            let
                ( alert, error ) =
                    updateEditor model.alert field value
            in
                { model
                    | alert = alert
                    , errors = (error ++ model.errors)
                }

        Error err ->
            { model | errors = (ErrorInfo field err :: model.errors) }


updateEditor : Alert -> Field -> String -> ( Alert, List ErrorInfo )
updateEditor alert field value =
    case field of
        Id ->
            case String.toInt value of
                Err msg ->
                    ( alert, [ ErrorInfo field msg ] )

                Ok value ->
                    ( { alert | id = value }, [] )

        Note ->
            ( { alert | note = value }, [] )

render mdl model mdlMsg epmsg =
    let
        alert =
            model.alert
    in
        grid []
            [ cell [ size All 1 ]
                [ renderInput mdl 1 Id (toString alert.id) (findFieldError model Id) mdlMsg epmsg
                ]
            , cell [ offset All 2, size All 1 ]
                [ renderInput mdl 2 Note alert.note (findFieldError model Note) mdlMsg epmsg
                ]
            ]

findFieldError : Model -> Field -> Maybe String
findFieldError model field =
    let
        ( a, b ) =
            List.partition (\aa -> aa.field == field) model.errors
    in
        List.head (List.map (\x -> x.error) a)



renderInput mdl id field value error mdlMsg epMsg =
    Textfield.render
        mdlMsg
        [ id ]
        mdl
        (fieldProperties field value epMsg error)
        []

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
