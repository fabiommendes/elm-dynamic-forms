module DynForms.Validation
    exposing
        ( defaultErrorMsg
        , maxLength
        , maxValue
        , minLength
        , minValue
        , withMsg
        )

{-| Validators are objects that represent an specific validation operation.


# Basic validators

@docs maxValue, minValue, maxLength, minLength


# Additional options

@docs withMsg, defaultErrorMsg

-}

import DynForms exposing (FieldInfo, Validator(..))


type alias ValidatorFactory =
    FieldInfo -> ( Validator, String )


singleValidator : (a -> Validator) -> a -> ValidatorFactory
singleValidator constructor arg =
    \info ->
        let
            validator =
                constructor arg
        in
        ( validator, defaultErrorMsg validator )


{-| Sets the minimum numerical value a field can assume
-}
minValue : Float -> ValidatorFactory
minValue =
    singleValidator MinValue


{-| Sets the maximum numerical value a field can assume
-}
maxValue : Float -> ValidatorFactory
maxValue =
    singleValidator MaxValue


{-| Sets the minimum length value a field can assume
-}
minLength : Int -> ValidatorFactory
minLength =
    singleValidator MinLength


{-| Sets the maximum length value a field can assume
-}
maxLength : Int -> ValidatorFactory
maxLength =
    singleValidator MaxLength


{-| Sets the error string used in the validator.

This function is designed to be used in a pipeline as in

    validators =
        [ minValue 0 |> withMsg "Value must be positive" ]

-}
withMsg : String -> ValidatorFactory -> ValidatorFactory
withMsg msg factory =
    \info ->
        let
            ( validator, _ ) =
                factory info
        in
        ( validator, msg )


{-| Format message template using info object
-}
formatMsg : String -> FieldInfo -> String
formatMsg st info =
    st


{-| Return the default error message for the given validator
-}
defaultErrorMsg : Validator -> String
defaultErrorMsg val =
    case val of
        MaxLength n ->
            "Must be at most {value} characters long."

        MinLength n ->
            "Must be at least {value} characters long."

        MaxValue n ->
            "Value cannot exceed {value}."

        MinValue n ->
            "Value cannot be lower than {value}."
