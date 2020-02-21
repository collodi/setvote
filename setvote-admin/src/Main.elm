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
import Bootstrap.Form.Radio as Radio
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
    { newSet : NewSet
    , sets : List Set
    }

type alias NewSet =
    { name : String
    , expires : String
    , newColor : String
    , category : String
    , colors : List String
    }

type alias Set =
    { name : String
    , expires : String
    , category : String
    , colors : List String
    }

init : () -> (Model, Cmd Msg)
init _ =
      (initModel, Cmd.none)

initModel : Model
initModel =
    Model initNewSet []

initNewSet : NewSet
initNewSet =
    NewSet "" "" "" "boulder" []

newSetToJson : NewSet -> E.Value
newSetToJson newSet =
    E.object
        [ ("name", E.string newSet.name)
        , ("expires", E.string newSet.expires)
        , ("category", E.string newSet.category)
        , ("colors", E.list E.string newSet.colors)
        ]


-- UPDATE

type Msg
    = UpdateNewSet NewSetMsg

type NewSetMsg
    = Name String
    | CloseDate String
    | Category String
    | NewColor String
    | AddColor
    | DelColor String
    | AddSet

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateNewSet AddSet ->
            ( initModel
            , addSet (newSetToJson model.newSet) )

        UpdateNewSet newSetMsg ->
            ( { model | newSet = updateNewSet newSetMsg model.newSet }
            , Cmd.none )

updateNewSet : NewSetMsg -> NewSet -> NewSet
updateNewSet msg newSet =
    case msg of
        Name name ->
            { newSet | name = name }

        CloseDate date ->
            { newSet | expires = date }

        Category category ->
            { newSet | category = category }

        NewColor color ->
            { newSet | newColor = color }

        AddColor ->
            { newSet
                | colors = List.sort (newSet.newColor :: newSet.colors)
                , newColor = "" }

        DelColor color ->
            { newSet
                | colors = List.filter ((/=) color) newSet.colors
            }

        _ -> newSet


-- VIEW

view : Model -> Html Msg
view model =
    Grid.container []
        [ Html.map UpdateNewSet (viewNewSet model.newSet)
        ]

viewNewSet : NewSet -> Html NewSetMsg
viewNewSet newSet =
    div []
        [ newVoteForm newSet
        , div [ Spacing.m2 ] []
        , ListGroup.ul (List.map viewColor newSet.colors)
        ]

viewColor : String -> ListGroup.Item NewSetMsg
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

newVoteForm : NewSet -> Html NewSetMsg
newVoteForm newSet =
    Grid.row []
        [ Grid.col []
            [ Form.label [ for "name" ] [ text "Name" ]
            , Input.text [ Input.id "name", Input.attrs [ value newSet.name, onInput Name ] ]
            ]
        , Grid.colBreak [ Spacing.m1 ]
        , Grid.col []
            [ Form.label [ for "close-date" ] [ text "Close Date" ]
            , Input.date [ Input.id "close-date", Input.attrs [ value newSet.expires, onInput CloseDate ] ]
            ]
        , Grid.colBreak [ Spacing.m1 ]
        , Grid.col []
            [ Form.label [ for "category" ] [ text "Category" ]
            , Radio.radio
                [ Radio.id "category"
                , Radio.checked (newSet.category == "boulder")
                , Radio.onClick (Category "boulder")
                ]
                "Boulder"
            , Radio.radio
                [ Radio.id "category"
                , Radio.checked (newSet.category == "rope")
                , Radio.onClick (Category "rope")
                ]
                "Rope"
            ]
        , Grid.colBreak [ Spacing.m1 ]
        , Grid.col []
            [ Form.label [ for "new-color" ] [ text "Add Color" ]
            , Input.text [ Input.id "new-color", Input.attrs [ value newSet.newColor, onInput NewColor ] ]
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
