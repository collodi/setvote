port module Login exposing (..)

import Browser
import Html exposing (Html, text, div, span, h5, hr)
import Html.Attributes exposing (value, for, style)
import Html.Events exposing (onInput, onClick)

import Bootstrap.Alert as Alert

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input

import Bootstrap.Button as Button

import Bootstrap.Utilities.Spacing as Spacing

import Json.Encode as E

port cmd : E.Value -> Cmd msg


-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL

type alias Model =
    { email : String
    , password : String
    }

init : () -> (Model, Cmd Msg)
init _ =
      (initModel, Cmd.none)

initModel : Model
initModel =
    Model "" ""

logInToJson : Model -> E.Value
logInToJson model =
    E.object
        [ ("action", E.string "login")
        , ("email", E.string model.email)
        , ("password", E.string model.password)
        ]

signUpToJson : Model -> E.Value
signUpToJson model =
    E.object
        [ ("action", E.string "signup")
        , ("email", E.string model.email)
        , ("password", E.string model.password)
        ]


-- UPDATE

type Msg
    = Email String
    | Password String
    | LogIn
    | SignUp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Email email ->
            ( { model | email = email }
            , Cmd.none )

        Password password ->
            ( { model | password = password }
            , Cmd.none )

        LogIn ->
            ( model
            , cmd (logInToJson model) )

        SignUp ->
            ( model
            , cmd (signUpToJson model) )

-- VIEW

view : Model -> Html Msg
view model =
    Grid.container [ Spacing.my3 ]
        [ Grid.row []
            [ Grid.col []
                [ Form.label [ for "email" ] [ text "Email" ]
                , Input.text
                    [ Input.id "email", Input.attrs [ value model.email, onInput Email ] ]
                ]
            , Grid.colBreak []
            , Grid.col []
                [ Form.label [ for "password" ] [ text "Password" ]
                , Input.text
                    [ Input.id "password", Input.attrs [ value model.password, onInput Password ] ]
                ]
            , Grid.colBreak []
            , Grid.col []
                [ Button.button
                    [ Button.primary, Button.block, Button.attrs [ onClick LogIn ] ]
                    [ text "Log In" ]
                ]
            , Grid.colBreak []
            , Grid.col []
                [ Button.button
                    [ Button.secondary, Button.block, Button.attrs [ onClick SignUp ] ]
                    [ text "Sign Up" ]
                ]
            ]
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
