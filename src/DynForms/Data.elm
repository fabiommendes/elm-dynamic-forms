module DynForms.Data
    exposing
        ( FieldData(..)
        , FieldDataType(..)
        , asBool
        , asFloat
        , asInt
        , asMaybeBool
        , asMaybeFloat
        , asMaybeInt
        , asMaybeString
        , asString
        )

{-| Converters for the basic FieldData type

@docs asBool, asFloat, asInt, asMaybeBool, asMaybeFloat, asMaybeInt 
@docs asMaybeString, asString

-}

--- TYPES ----------------------------------------------------------------------


{-| Field data type.
-}
type FieldDataType
    = StringDataType
    | BoolDataType
    | IntDataType
    | FloatDataType


{-| Field data
-}
type FieldData
    = String String
    | Bool Bool
    | Int Int
    | Float Float
    | Empty



--- CONVERSION FUNCTIONS  ------------------------------------------------------


{-| Coerce data to String, using a sensible default value if the field data is
not in the expected state.
-}
asString : FieldData -> String
asString data =
    case data of
        String x ->
            x

        Empty ->
            ""

        Int x ->
            toString x

        Float x ->
            toString x

        Bool x ->
            toString x


{-| Coerce data to Bool, using a sensible default value if the field data is
not in the expected state.
-}
asBool : FieldData -> Bool
asBool data =
    case data of
        Bool x ->
            x

        String x ->
            x /= ""

        Float x ->
            x /= 0.0

        Int x ->
            x /= 0

        Empty ->
            False


{-| Coerce data to Int, using a sensible default value if the field data is
not in the expected state.
-}
asInt : FieldData -> Int
asInt data =
    case data of
        Int x ->
            x

        _ ->
            asFloat data |> truncate


{-| Coerce data to Float, using a sensible default value if the field data is
not in the expected state.
-}
asFloat : FieldData -> Float
asFloat data =
    case data of
        Float x ->
            x

        Int x ->
            toFloat x

        String x ->
            String.toFloat x
                |> Result.toMaybe
                |> Maybe.withDefault 0.0

        Bool x ->
            case x of
                True ->
                    1.0

                False ->
                    0.0

        Empty ->
            0.0


{-| Convert FieldData to String, if it is in the correct state.
-}
asMaybeString : FieldData -> Maybe String
asMaybeString data =
    case data of
        String x ->
            Just x

        _ ->
            Nothing


{-| Convert FieldData to Bool, if it is in the correct state.
-}
asMaybeBool : FieldData -> Maybe Bool
asMaybeBool data =
    case data of
        Bool x ->
            Just x

        _ ->
            Nothing


{-| Convert FieldData to Int, if it is in the correct state.
-}
asMaybeInt : FieldData -> Maybe Int
asMaybeInt data =
    case data of
        Int x ->
            Just x

        _ ->
            Nothing


{-| Convert FieldData to Float, if it is in the correct state.
-}
asMaybeFloat : FieldData -> Maybe Float
asMaybeFloat data =
    case data of
        Float x ->
            Just x

        _ ->
            Nothing
