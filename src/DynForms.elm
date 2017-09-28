module DynForms
    exposing
        ( FieldInfo
        , FieldType(..)
        , Form
        , FormLayout(..)
        , Msg(..)
        , Validator(..)
        , form
        , getFieldDefaultAt
        , getFieldHelpAt
        , getFieldInfoAt
        , getFieldLabelAt
        , getFieldPlaceholderAt
        , getFieldTypeAt
        , getFieldValidatorsAt
        , updateForm
        )

{-| Dynamic forms can be created from JSON, using Elm to control user
interaction and basic validation. Data can be serialized and sent to the
backend for further validation and persistence.


# Types

@docs Form, FieldType, FormLayout, FieldInfo, Validator, form


# Field constructors and helpers

A field is declared using functions in the DynForms.Fields module. This module
defines a few functions used to extract information for specific fields on a
form.

@docs getFieldInfoAt, getFieldLabelAt, getFieldDefaultAt, getFieldPlaceholderAt
@docs getFieldValidatorsAt, getFieldHelpAt, getFieldTypeAt


# Validators

Field validators are declared in the DynForms.Validators module.


# View Functions

View functions are declared in the DynForms.View module.


# Elm Archtecture

@docs Msg, updateForm

-}

import Dict exposing (Dict)
import DynForms.Data exposing (..)
import DynForms.State exposing (..)
import Form
import Form.Field
import Maybe exposing (withDefault)


--- BASIC DATA TYPES


{-| Basic form type.

The Form type represents a full form in the application including the filled in
values, declarations of field types and behaviors and the form layout.

Forms can be safely included in a model. It replicates a standard Elm
archtecture and provides standard functions and messages such as updateForm,
viewForm and FormMsg.

You should create new forms using the DynForms.form constructor.

-}
type alias Form =
    { action : String
    , ajax : Bool
    , fields : Dict String FieldInfo
    , layout : FormLayout
    , state : FormState
    }


{-| Represents a layout of form elements.

The Layout describes how form elements will be placed on the page when creating
a view.

-}
type FormLayout
    = LinearLayout (List String)
    | TableLayout (List String)


{-| Stores basic information about a field
-}
type alias FieldInfo =
    { id : String
    , fieldType : FieldType
    , dataType : FieldDataType
    , label : Maybe String
    , placeholder : Maybe String
    , default : Maybe FieldData
    , help : Maybe String
    , validators : List ( Validator, String )
    }


{-| Field type.
-}
type FieldType
    = HiddenField
    | StringField
    | TextAreaField
    | IntField
    | FloatField
    | BoolField


{-| Represents a field value validator
-}
type Validator
    = MinValue Float
    | MaxValue Float
    | MinLength Int
    | MaxLength Int


{-| Form messages
-}
type Msg
    = StateMsg Form.Msg


{-| Alias for clarity and type safety
-}
type alias Id =
    String



--- CONSTRUCTORS ---------------------------------------------------------------


{-| Creates a new form
-}
form : String -> List FieldInfo -> Form
form action info =
    { action = action
    , ajax = False
    , fields = Dict.fromList <| List.map (\info -> ( info.id, info )) info
    , layout = LinearLayout <| List.map (\info -> info.id) info
    , state = Form.initial (getInitial info) (getValidations info)
    }



--- FIELD INFO DATA ACCESSORS --------------------------------------------------


{-| Retrive information from a field if it exists
-}
getFieldInfoAt : Id -> Form -> Maybe FieldInfo
getFieldInfoAt id form =
    Dict.get id form.fields


{-| Get field type for field with the given id, if it exists.
-}
getFieldTypeAt : Id -> Form -> Maybe FieldType
getFieldTypeAt id form =
    getFieldInfoAt id form
        |> Maybe.map (\info -> info.fieldType)


{-| Get field label for field with the given id, if it exists and label is
defined.
-}
getFieldLabelAt : Id -> Form -> Maybe String
getFieldLabelAt id form =
    getFieldInfoAt id form
        |> Maybe.andThen (\info -> info.label)


{-| Get field placeholder for field with the given id, if it exists and
placeholder is defined.
-}
getFieldPlaceholderAt : Id -> Form -> Maybe String
getFieldPlaceholderAt id form =
    getFieldInfoAt id form
        |> Maybe.andThen (\info -> info.placeholder)


{-| Get field default value for field with the given id, if it exists and
default value is defined.
-}
getFieldDefaultAt : Id -> Form -> Maybe FieldData
getFieldDefaultAt id form =
    getFieldInfoAt id form
        |> Maybe.andThen (\info -> info.default)


{-| Get field help string for field with the given id, if it exists and the
help string is defined.
-}
getFieldHelpAt : Id -> Form -> Maybe String
getFieldHelpAt id form =
    getFieldInfoAt id form
        |> Maybe.andThen (\info -> info.help)


{-| Get list of validators for the given field. If field does not exist,
return an empty list.
-}
getFieldValidatorsAt : Id -> Form -> List ( Validator, String )
getFieldValidatorsAt id form =
    getFieldInfoAt id form
        |> Maybe.map (\info -> info.validators)
        |> Maybe.withDefault []



--- DATA ACCESSORS -------------------------------------------------------------


getStringAt : Id -> Form -> String
getStringAt id form =
    let
        info =
            getFieldInfoAt id form
    in
    (Form.getFieldAsString id form.state).value |> withDefault ""



--- TEA FUNCTIONS --------------------------------------------------------------


{-| Update form using message
-}
updateForm : Msg -> Form -> Form
updateForm msg form =
    case msg of
        StateMsg stateMsg ->
            case stateMsg of
                Form.NoOp ->
                    form

                _ ->
                    let
                        validations =
                            Dict.values form.fields
                                |> List.map (\x -> ( x.id, x.dataType, x.default ))
                                |> validationFromFields
                    in
                    { form
                        | state = Form.update validations stateMsg form.state
                    }
