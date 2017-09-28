module DynForms.Field
    exposing
        ( default
        , help
        , hiddenField
        , intField
        , label
        , placeholder
        , stringField
        , validators
        )

{-|


# Field types

@docs intField, stringField, hiddenField


## Constructor helpers

The functions bellow create modified versions of the field setting up one of
its parameters. They are useful for initializing the field object using the
pipeline notation as in

    myField =
        intField "field-id"
            |> label "Number"
            |> placeholder "42"
            |> default "0"

@docs label, placeholder, help, default, validators

-}

import DynForms exposing (FieldInfo, FieldType(..), Validator)
import DynForms.Data exposing (FieldData, FieldDataType(..))
import Form.Field exposing (FieldValue(..))


--- FIELD TYPES --------------------------------------------------------------


defaultFieldInfo : FieldInfo
defaultFieldInfo =
    { id = ""
    , fieldType = HiddenField
    , dataType = StringDataType
    , label = Nothing
    , placeholder = Nothing
    , default = Nothing
    , help = Nothing
    , validators = []
    }



--- BASIC FIELD CONSTRUCTORS ---------------------------------------------------


{-| String fields represents a simple single line string input.
-}
stringField : String -> FieldInfo
stringField id =
    { defaultFieldInfo
        | id = id
        , fieldType = StringField
    }


{-| Text fields are similar to regular string fields, but uses a TextArea as
input .
-}
textField : String -> FieldInfo
textField id =
    { defaultFieldInfo
        | id = id
        , fieldType = StringField
    }


{-| Hidden field possess a string value that is hidden from the user.
-}
hiddenField : String -> FieldInfo
hiddenField id =
    { defaultFieldInfo | id = id, fieldType = HiddenField }


{-| Int fields hold numeric data as whole numbers.
-}
intField : String -> FieldInfo
intField id =
    { defaultFieldInfo
        | id = id
        , fieldType = IntField
        , dataType = IntDataType
    }


{-| Float fields hold numeric data that can have decimal places.
-}
floatField : String -> FieldInfo
floatField id =
    { defaultFieldInfo
        | id = id
        , fieldType = FloatField
        , dataType = FloatDataType
    }


{-| Boolean fields hold a boolean value and are usually represented by a
checkbox.
-}
boolField : String -> FieldInfo
boolField id =
    { defaultFieldInfo
        | id = id
        , fieldType = BoolField
        , dataType = BoolDataType
    }



--- CONSTRUCTOR HELPERS --------------------------------------------------------


type alias FieldMod m =
    m -> FieldInfo -> FieldInfo


{-| Sets the label text of a field.

The label is usually place just above or to the left of a field input box
to tell the user what kind of information should be filled in.

-}
label : FieldMod String
label st info =
    { info | label = Just st }


{-| Sets the placeholder text of a field.

The placeholder is a text that is displayed inside the input field and is
replaced by the user input as soon as the user starts typing. A common design
choice is to replace the label with a placeholder. DynForms treats both
independently.

-}
placeholder : FieldMod String
placeholder st info =
    { info | placeholder = Just st }


{-| Sets the default value of a field.

This pre-fills the field with some value. If this option is enabled, the
placeholder should not appear unless the user deletes the field content.

-}
default : FieldMod FieldData
default value info =
    { info | default = Just value }


{-| Defines a help string that is usually displayed bellow the field input
element.

Help strings are useful UI elements that can be used to provide additional
information to the user on how to fill the field or any other type of
clarification.

-}
help : FieldMod String
help st info =
    { info | help = Just st }


{-| Defines a list of validators for a field declaration.

Each validator is declared using the functions in the DynForms.Validators
module.

-}
validators : List (FieldInfo -> ( Validator, String )) -> FieldInfo -> FieldInfo
validators validators info =
    let
        extra : List ( Validator, String )
        extra =
            List.map (\f -> f info) validators
    in
    { info | validators = info.validators ++ extra }
