port module Login exposing (..)

import Browser
import Html exposing (Html, text, div, span, h5, hr)
import Html.Attributes exposing (value, for, style, href)
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
import Json.Decode as D

port cmd : E.Value -> Cmd msg
port message : (D.Value -> msg) -> Sub msg
port authd : (D.Value -> msg) -> Sub msg


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
    , msg : String
    , authd : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
      (initModel, Cmd.none)

initModel : Model
initModel =
    Model "" "" "" False

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

signOutToJson : E.Value
signOutToJson =
    E.object [ ("action", E.string "signout" ) ]


-- UPDATE

type Msg
    = Email String
    | Password String
    | LogIn
    | SignUp
    | ShowMsg D.Value
    | Authd D.Value
    | SignOut


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
            if String.isEmpty model.email then
                ( { model | msg = "What's your email address?" }
                , Cmd.none )
            else if String.isEmpty model.password then
                ( { model | msg = "What's your password?" }
                , Cmd.none )
            else
                ( model, cmd (logInToJson model) )

        SignUp ->
            if String.isEmpty model.email then
                ( { model | msg = "What's your email address?" }
                , Cmd.none )
            else if String.isEmpty model.password then
                ( { model | msg = "What's your password?" }
                , Cmd.none )
            else
                ( model, cmd (signUpToJson model) )

        ShowMsg value ->
            case D.decodeValue D.string value of
                Ok msg_ ->
                    ( { model | msg = msg_ }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "Error decoding message." }
                    , Cmd.none )

        Authd value ->
            case D.decodeValue D.bool value of
                Ok authd_ ->
                    ( { model | authd = authd_ }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "Error decoding authd." }
                    , Cmd.none )

        SignOut ->
            ( { model | email = "", password = "" }
            , cmd signOutToJson )


-- VIEW

view : Model -> Html Msg
view model =
    if model.authd then
        viewLoggedIn model
    else
        viewLogin model

viewLoggedIn : Model -> Html Msg
viewLoggedIn model =
    Grid.container [ Spacing.my3 ]
        [ Grid.row []
            [ Grid.col []
                [ Button.linkButton
                    [ Button.primary, Button.block, Button.attrs [ href "/admin.html" ] ]
                    [ text "Go To Admin Page" ]
                ]
            , Grid.colBreak [ Spacing.my2 ]
            , Grid.col []
                [ Button.button
                    [ Button.secondary, Button.block, Button.attrs [ onClick SignOut ] ]
                    [ text "Log Out" ]
                ]
            ]
        ]

viewLogin : Model -> Html Msg
viewLogin model =
    Grid.container [ Spacing.my3 ]
        [ Grid.row []
            [ Grid.col []
                [ Form.label [ for "email" ] [ text "Email" ]
                , Input.text
                    [ Input.id "email", Input.attrs [ value model.email, onInput Email ] ]
                ]
            , Grid.colBreak [ Spacing.my1 ]
            , Grid.col []
                [ Form.label [ for "password" ] [ text "Password" ]
                , Input.text
                    [ Input.id "password", Input.attrs [ value model.password, onInput Password ] ]
                ]
            , Grid.colBreak [ Spacing.my3 ]
            , Grid.col []
                [ Button.button
                    [ Button.primary, Button.block, Button.attrs [ onClick LogIn ] ]
                    [ text "Log In" ]
                ]
            , Grid.colBreak [ Spacing.my2 ]
            , Grid.col []
                [ Button.button
                    [ Button.secondary, Button.block, Button.attrs [ onClick SignUp ] ]
                    [ text "Sign Up" ]
                ]
            , Grid.colBreak [ Spacing.my3 ]
            , Grid.col [] (
                if String.isEmpty model.msg then
                    []
                else
                    [ Alert.simpleDanger [] [ text model.msg ] ]
                )
            ]
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ message ShowMsg
        , authd Authd
        ]
