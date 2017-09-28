module DynForms.View
    exposing
        ( viewForm
        )

{-|


# View functions

@docs viewForm

-}

import Dict
import DynForms exposing (..)
import DynForms.Field exposing (..)
import DynForms.State exposing (..)
import Form
import Form.Field exposing (FieldValue(..))
import Form.Input as Input
import Html exposing (Html, div, form, text)
import Html.Attributes exposing (action, class, name, step, type_)
import Maybe exposing (withDefault)


{-| Render form as Elm HTML
-}
viewForm : Form -> Html Msg
viewForm form =
    case form.layout of
        LinearLayout ids ->
            viewLinearLayout ids form

        TableLayout ids ->
            viewLinearLayout ids form


errorDecl : FieldInfo
errorDecl =
    hiddenField ""



--- LINEAR LAYOUT --------------------------------------------------------------


viewLinearLayout : List String -> Form -> Html Msg
viewLinearLayout ids form =
    let
        info =
            List.map (\id -> Dict.get id form.fields |> withDefault errorDecl) ids

        values =
            List.map (\id -> getFormValue id form) ids
    in
    Html.form [ class "form", action form.action ] <|
        List.map2 (viewField form.state) info values


viewField : FormState -> FieldInfo -> FieldValue -> Html Msg
viewField state info data =
    let
        label =
            case info.label of
                Just st ->
                    [ Html.label [] [ text st ] ]

                Nothing ->
                    []

        helpText =
            case info.help of
                Just st ->
                    [ Html.br [] []
                    , Html.span [ class "form-help-text" ] [ text st ]
                    ]

                Nothing ->
                    []

        children =
            label ++ [ getInputElement state info data ] ++ helpText
    in
    Html.p [] children


getInputElement : FormState -> FieldInfo -> FieldValue -> Html Msg
getInputElement state info st =
    let
        fromText attrs =
            Form.getFieldAsString info.id state
                |> (\state -> Input.textInput state (name info.id :: attrs))
                |> Html.map StateMsg
    in
    case info.fieldType of
        IntField ->
            fromText [ type_ "number", step "1" ]

        FloatField ->
            fromText [ type_ "number" ]

        BoolField ->
            Form.getFieldAsBool info.id state
                |> (\state -> Input.checkboxInput state [])
                |> Html.map StateMsg

        _ ->
            fromText []


getFormValue : String -> Form -> FieldValue
getFormValue id form =
    (Form.getFieldAsString id form.state).value
        |> Maybe.map (\st -> String st)
        |> withDefault EmptyField
