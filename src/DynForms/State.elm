module DynForms.State
    exposing
        ( FormData
        , FormErrors
        , FormState
        , FormValidation
        , initialState
        , validationFromFields
        )

{-| Types and functions in this modules declares how form data is represented
and accessed internally.

This is a private module.

-}

import DynForms.Data exposing (..)
import Form
import Form.Field as FormField
import Form.Validate as Validate exposing (Validation)


type alias FormData =
    List ( String, FieldData )


{-| How form errors are represented internally
-}
type alias FormErrors =
    List String


{-| Represents data inside a form
-}
type alias FormState =
    Form.Form FormErrors FormData


type alias FormValidation =
    Validation FormErrors FormData


type alias FieldInfo =
    ( String, FieldDataType, Maybe FieldData )


{-| Creates a validation object from form data
-}
validationFromFields : List FieldInfo -> Validation FormErrors FormData
validationFromFields fields =
    List.map validationFromField fields
        |> Validate.sequence


validationFromField : FieldInfo -> Validation FormErrors ( String, FieldData )
validationFromField ( id, dataType, default ) =
    let
        pair validator =
            Validate.map (\v -> ( id, v )) validator
    in
    case dataType of
        StringDataType ->
            pair validateString

        FloatDataType ->
            pair validateFloat

        IntDataType ->
            pair validateInt

        BoolDataType ->
            pair validateBool


validateString : Validation FormErrors FieldData
validateString =
    Validate.customValidation Validate.string (\x -> Ok (String x))


validateInt : Validation FormErrors FieldData
validateInt =
    Validate.customValidation Validate.int (\x -> Ok (Int x))


validateFloat : Validation FormErrors FieldData
validateFloat =
    Validate.customValidation Validate.float (\x -> Ok (Float x))


validateBool : Validation FormErrors FieldData
validateBool =
    Validate.customValidation Validate.bool (\x -> Ok (Bool x))



--- STATE CONSTRUCTORS ---------------------------------------------------------


initialStateValues : List FieldInfo -> List ( String, FormField.Field )
initialStateValues lst =
    let
        initial ( dataType, default ) =
            case dataType of
                StringDataType ->
                    FormField.string ""

                _ ->
                    FormField.value FormField.EmptyField
    in
    List.map (\( id, x, y ) -> ( id, initial ( x, y ) )) lst


initialState : List FieldInfo -> FormState
initialState lst =
    let
        values =
            initialStateValues lst

        validators =
            validationFromFields lst
    in
    Form.initial values validators
