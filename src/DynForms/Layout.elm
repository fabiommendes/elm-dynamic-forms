module DynForms.Layout
    exposing
        ( forceLinearLayout
        , forceTableLayout
        , linearLayout
        , tableLayout
        )

{-| Functions used to define the layout of form elements.


# Layout types

@docs linearLayout, tableLayout


# Conversion between different layouts

@docs forceLinearLayout , forceTableLayout

-}

import DynForms exposing (Form, FormLayout(..))


--- CONSTRUCTORS ---------------------------------------------------------------


{-| Declares a linear layout
-}
linearLayout : List String -> FormLayout
linearLayout lst =
    LinearLayout lst


{-| Declares a linear layout
-}
tableLayout : List String -> FormLayout
tableLayout lst =
    LinearLayout lst



--- LAYOUT CONVERSIONS ---------------------------------------------------------


{-| Converts any form layout to a linear layout
-}
forceLinearLayout : Form -> FormLayout
forceLinearLayout form =
    linearLayout (getLayoutIds form.layout)


{-| Converts any form layout to a linear table layout
-}
forceTableLayout : Form -> FormLayout
forceTableLayout form =
    linearLayout (getLayoutIds form.layout)


{-| Gets a linearized list of ids from the layout element
-}
getLayoutIds : FormLayout -> List String
getLayoutIds layout =
    case layout of
        LinearLayout ids ->
            ids

        TableLayout ids ->
            ids
