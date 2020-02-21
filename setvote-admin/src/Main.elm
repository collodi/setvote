port module Main exposing (..)

import Browser
import Html exposing (Html, text, button, div, input, span)
import Html.Attributes exposing (type_, value, for)
import Html.Events exposing (onInput, onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.ListGroup as ListGroup

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

import Bootstrap.Utilities.Spacing as Spacing

import Json.Encode as E

port addSet : E.Value -> Cmd msg


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
    { name : String
    , expires : String
    , newColor : String
    , colors : List String
    }

init : () -> (Model, Cmd Msg)
init _ =
      (newModel, Cmd.none)

newModel : Model
newModel =
    Model "" "" "" []

toJson : Model -> E.Value
toJson model =
    E.object
        [ ("name", E.string model.name)
        , ("expires", E.string model.expires)
        , ("colors", E.list E.string model.colors)
        ]


-- UPDATE

type Msg
    = Name String
    | CloseDate String
    | NewColor String
    | AddColor
    | DelColor String
    | AddSet

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Name name ->
            ( { model | name = name }
            , Cmd.none )

        CloseDate date ->
            ( { model | expires = date }
            , Cmd.none )

        NewColor color ->
            ( { model | newColor = color }
            , Cmd.none )

        AddColor ->
            (
                { model
                    | colors = List.sort (model.newColor :: model.colors)
                    , newColor = ""
                }
            , Cmd.none )

        DelColor color ->
            (
                { model
                    | colors = List.filter ((/=) color) model.colors
                }
            , Cmd.none )

        AddSet ->
            ( newModel
            , addSet (toJson model) )




-- VIEW

view : Model -> Html Msg
view model =
    Grid.container []
        [ newVoteForm model
        , div [ Spacing.m2 ] []
        , ListGroup.ul (List.map viewColor model.colors)
        ]


viewColor : String -> ListGroup.Item Msg
viewColor color =
    ListGroup.li []
        [ Grid.row []
            [ Grid.col []
                [ div [] [ text color ] ]
            , Grid.col [ Col.xsAuto ]
                [ Button.button
                    [ Button.danger, Button.attrs [ onClick (DelColor color) ] ]
                    [ text "Delete" ]
                ]
            ]
        ]

newVoteForm : Model -> Html Msg
newVoteForm model =
    Grid.row []
        [ Grid.col []
            [ Form.label [ for "name" ] [ text "Name" ]
            , Input.text [ Input.id "name", Input.attrs [ value model.name, onInput Name ] ]
            ]
        , Grid.colBreak [ Spacing.m1 ]
        , Grid.col []
            [ Form.label [ for "close-date" ] [ text "Close Date" ]
            , Input.date [ Input.id "close-date", Input.attrs [ value model.expires, onInput CloseDate ] ]
            ]
        , Grid.colBreak [ Spacing.m1 ]
        , Grid.col []
            [ Form.label [ for "new-color" ] [ text "Add Color" ]
            , Input.text [ Input.id "new-color", Input.attrs [ value model.newColor, onInput NewColor ] ]
            , div [ Spacing.m1 ] []
            , Button.button
                [ Button.dark, Button.block, Button.attrs [ onClick AddColor ] ]
                [ text "Add Color" ]
            ]
        , Grid.colBreak [ Spacing.m3 ]
        , Grid.col []
            [ Button.button
                [ Button.primary, Button.block, Button.attrs [ onClick AddSet ] ]
                [ text "Add Set" ]
            ]
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
