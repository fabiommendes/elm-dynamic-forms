module DynForms.JsonEncode exposing (encodeString, encodeValue)

{-| Functions that encode form or parts of the form to JSON.

The most useful functions are the encodeValue that encodes the complete form
and encodeValueData that only encodes the current state of form data.

@docs encodeValue, encodeString


# Additional encoders

-}

import Dict exposing (Dict)
import DynForms exposing (..)
import DynForms.Data exposing (..)
import DynForms.State exposing (..)
import Form
import Json.Encode as Enc
    exposing
        ( Value
        , array
        , bool
        , encode
        , float
        , int
        , list
        , null
        , object
        , string
        )
import Maybe


{-| Encode a form to a JSON value.
-}
encodeValue : Form -> Value
encodeValue form =
    let
        ids =
            Dict.keys form.fields

        fields =
            Dict.values form.fields
    in
    object
        [ ( "action", string form.action )
        , ( "fields", encodeFields ids fields )
        , ( "data", encodeData ids fields form.state )
        , ( "layout", encodeLayout form.layout )
        ]


{-| Encode a form to a JSON string.
-}
encodeString : Int -> Form -> String
encodeString indent form =
    encode indent (encodeValue form)


encodeFields : List String -> List FieldInfo -> Value
encodeFields ids fields =
    let
        encodePair id field =
            ( id, encodeField field )
    in
    object <| List.map2 encodePair ids fields


encodeField : FieldInfo -> Value
encodeField field =
    let
        type_ =
            [ ( "type", string (typeName field.fieldType) ) ]

        label =
            case field.label of
                Just st ->
                    [ ( "label", string st ) ]

                Nothing ->
                    []

        placeholder =
            case field.placeholder of
                Just st ->
                    [ ( "placeholder", string st ) ]

                Nothing ->
                    []

        help =
            case field.help of
                Just st ->
                    [ ( "help", string st ) ]

                Nothing ->
                    []

        validators =
            [ ( "validators", list [] ) ]
    in
    object <| type_ ++ label ++ placeholder ++ help ++ validators


typeName : FieldType -> String
typeName tt =
    case tt of
        HiddenField ->
            "hidden"

        StringField ->
            "string"

        TextAreaField ->
            "textarea"

        FloatField ->
            "float"

        IntField ->
            "integer"

        BoolField ->
            "boolean"


encodeData : List String -> List FieldInfo -> FormState -> Value
encodeData ids fields state =
    let
        encodeItem id field =
            ( id, encodeDatum id field state )
    in
    object <| List.map2 encodeItem ids fields


encodeDatum : String -> FieldInfo -> FormState -> Value
encodeDatum id info state =
    case info.dataType of
        StringDataType ->
            (Form.getFieldAsString id state).value
                |> Maybe.map string
                |> Maybe.withDefault (string "")

        IntDataType ->
            (Form.getFieldAsString id state).value
                |> Maybe.andThen (\x -> String.toInt x |> Result.toMaybe)
                |> Maybe.map int
                |> Maybe.withDefault null

        FloatDataType ->
            (Form.getFieldAsString id state).value
                |> Maybe.andThen (\x -> String.toFloat x |> Result.toMaybe)
                |> Maybe.map float
                |> Maybe.withDefault null

        BoolDataType ->
            (Form.getFieldAsBool id state).value
                |> Maybe.map bool
                |> Maybe.withDefault null


encodeLayout : FormLayout -> Value
encodeLayout layout =
    case layout of
        LinearLayout ids ->
            object
                [ ( "type", string "linear" )
                , ( "fields", list <| List.map string ids )
                ]

        TableLayout ids ->
            object
                [ ( "type", string "table" )
                , ( "fields", list <| List.map string ids )
                ]
