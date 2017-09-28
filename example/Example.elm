module Main exposing (..)

import DynForms exposing (..)
import DynForms.Field exposing (..)
import DynForms.JsonDecode exposing (..)
import DynForms.JsonEncode exposing (..)
import DynForms.Validation exposing (..)
import DynForms.View exposing (..)
import Html exposing (Html, code, div, h1, h2, p, pre, text)


-- Declares a form


sampleForm : Form
sampleForm =
    form "/api/form/1"
        [ stringField "name"
            |> label "Name"
            |> placeholder "Full name"
            |> help "Your name and family name in the order you'd like to be called."
            |> validators [ maxLength 200 ]
        , intField "age"
            |> label "Age"
            |> help "Your age"
            |> validators [ minValue 0, maxValue 120 ]
        ]



-- Setup form validation


init : ( Form, Cmd Msg )
init =
    ( sampleForm, Cmd.none )



-- Render form with Input helpers


view : Form -> Html Msg
view form =
    let
        json =
            encodeString 4 form

        rebuiltForm =
            decodeString json

        fromJsonForm =
            case rebuiltForm of
                Ok form ->
                    viewForm form

                Err msg ->
                    p [] [ text msg ]
    in
    div []
        [ h1 [] [ text "Dynamic forms example" ]
        , h2 [] [ text "Here is the form" ]
        , viewForm form
        , h2 [] [ text "Its representation as a JSON string" ]
        , code []
            [ pre [] [ text json ]
            ]
        , h2 [] [ text "Form reconstructed from JSON string input" ]
        , fromJsonForm
        ]


main =
    Html.program
        { init = init
        , update = \msg f -> ( updateForm msg f, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }
