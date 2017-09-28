module DynForms.JsonDecode exposing (decodeString, decodeValue)

{-| Functions that decode a JSON string to a form object.

@docs decodeForm


# Additional encoders

-}

import Dict exposing (Dict)
import DynForms exposing (..)
import DynForms.Data exposing (..)
import DynForms.Field exposing (..)
import DynForms.State exposing (..)
import DynForms.Util as Util
import DynForms.Validation exposing (..)
import Form
import Json.Decode as Dec exposing (..)


{-| Decode a JSON value to its corresponding form object
-}
decodeValue : Value -> Result String Form
decodeValue value =
    Dec.decodeValue formDecoder value


{-| Decode a JSON string to its corresponding form object
-}
decodeString : String -> Result String Form
decodeString st =
    Dec.decodeString formDecoder st



--- DECODERS -------------------------------------------------------------------


formDecoder : Decoder Form
formDecoder =
    let
        formFromData action ajax info layout state =
            let
                toInfoTuple x =
                    ( x.id, x.dataType, x.default )

                updateFields x =
                    { x
                        | ajax = ajax
                        , layout = layout
                        , state = initialState (List.map toInfoTuple info)
                    }
            in
            form action info
                |> updateFields
    in
    map5 formFromData
        (field "action" string)
        (oneOf [ field "ajax" bool, succeed True ])
        (field "fields" fieldListDecoder)
        (field "layout" layoutDecoder)
        (field "state" stateDecoder)


fieldListDecoder : Decoder (List FieldInfo)
fieldListDecoder =
    let
        transferId ( id, info ) =
            { info | id = id }
    in
    keyValuePairs fieldInfoDecoder
        |> andThen (\lst -> succeed (List.map transferId lst))


layoutDecoder : Decoder FormLayout
layoutDecoder =
    let
        layoutBuilder tt fields =
            case tt of
                "table" ->
                    Ok (TableLayout fields)

                "linear" ->
                    Ok (LinearLayout fields)

                _ ->
                    Err ("Invalid layout: " ++ tt)
    in
    map2 layoutBuilder
        (field "type" string)
        (field "fields" (list string))
        |> andThen
            (\x ->
                case x of
                    Ok value ->
                        succeed value

                    Err msg ->
                        fail msg
            )


stateDecoder : Decoder (List ( String, FieldData ))
stateDecoder =
    let
        decodeValuePair : ( String, Value ) -> ( String, FieldData )
        decodeValuePair ( name, v ) =
            let
                data =
                    String "decoded"
            in
            ( name, data )
    in
    keyValuePairs value
        |> andThen (\x -> succeed (List.map decodeValuePair x))



--- VALIDATORS -----------------------------------------------------------------


validatorDecoder : Decoder ( Validator, String )
validatorDecoder =
    let
        single : Decoder a -> List Value -> Result String a
        single decoder lst =
            case lst of
                [] ->
                    Err "Validator is missing an argument"

                n :: [] ->
                    Dec.decodeValue decoder n

                _ ->
                    Err "Validator took too many arguments"

        decodeFromExpr : List Value -> String -> Decoder ( Validator, String )
        decodeFromExpr tail head =
            let
                ( name, colon, error_ ) =
                    Util.partition ":" head

                error =
                    String.trimLeft error_
            in
            case name of
                "max-value" ->
                    validateValidator
                        MaxValue
                        error
                        (single float tail)

                "min-value" ->
                    validateValidator
                        MinValue
                        error
                        (single float tail)

                "max-length" ->
                    validateValidator
                        MaxLength
                        error
                        (single int tail)

                "min-length" ->
                    validateValidator
                        MinLength
                        error
                        (single int tail)

                _ ->
                    fail <| "Invalid validator: " ++ name

        decodeFromList x =
            case x of
                [] ->
                    fail "Empty validator"

                head :: tail ->
                    string |> andThen (decodeFromExpr tail)
    in
    list value |> andThen decodeFromList


validateValidator : (a -> Validator) -> String -> Result String a -> Decoder ( Validator, String )
validateValidator constructor err arg =
    case arg of
        Ok x ->
            let
                validator =
                    constructor x
            in
            case err of
                "" ->
                    succeed ( validator, "error: " ++ toString validator )

                _ ->
                    succeed ( validator, err )

        Err msg ->
            fail msg



--- VALIDATORS -----------------------------------------------------------------


fieldInfoDecoder : Decoder FieldInfo
fieldInfoDecoder =
    map7 fieldInfoFactory
        (oneOf [ field "id" string, succeed "" ])
        (field "type" fieldTypeDecoder)
        (maybe (field "label" string))
        (maybe (field "placeholder" string))
        (maybe (field "help" string))
        (maybe (field "default" fieldDataDecoder))
        (field "validators" (list validatorDecoder))


fieldTypeDecoder : Decoder FieldType
fieldTypeDecoder =
    string
        |> andThen
            (\st ->
                case st of
                    "string" ->
                        succeed StringField

                    "textarea" ->
                        succeed TextAreaField

                    "integer" ->
                        succeed IntField

                    "float" ->
                        succeed FloatField

                    "boolean" ->
                        succeed BoolField

                    "hidden" ->
                        succeed HiddenField

                    _ ->
                        fail ("Invalid field type: " ++ st)
            )


fieldDataDecoder : Decoder FieldData
fieldDataDecoder =
    oneOf
        [ string |> andThen (\x -> succeed (String x))
        , float |> andThen (\x -> succeed (Float x))
        , int |> andThen (\x -> succeed (Int x))
        , bool |> andThen (\x -> succeed (Bool x))
        ]


fieldInfoFactory :
    String
    -> FieldType
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe FieldData
    -> List ( Validator, String )
    -> FieldInfo
fieldInfoFactory id type_ label placeholder help default validators =
    let
        dataType =
            case type_ of
                BoolField ->
                    BoolDataType

                IntField ->
                    IntDataType

                FloatField ->
                    FloatDataType

                _ ->
                    StringDataType

        base =
            hiddenField id
    in
    { base
        | fieldType = type_
        , dataType = dataType
        , label = label
        , placeholder = placeholder
        , help = help
        , default = default
        , validators = validators
    }
