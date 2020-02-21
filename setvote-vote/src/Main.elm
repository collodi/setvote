port module Main exposing (..)

import Browser
import Html exposing (Html, text, button, div, input, span, h1)
import Html.Attributes exposing (type_, value, for)
import Html.Events exposing (onInput, onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.Alert as Alert

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block

import Bootstrap.Form.Select as Select

import Bootstrap.Button as Button

import Json.Encode as E
import Json.Decode as D

port openSet : (D.Value -> msg) -> Sub msg
port castVote : E.Value -> Cmd msg


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
    { set : Set
    , routes : List Route
    , msg : String
    }

type alias Route =
    { color : String
    , grade : String
    }

type alias Set =
    { id : String
    , name : String
    , expires : String
    , colors : List String
    }

init : () -> (Model, Cmd Msg)
init _ =
      (newModel, Cmd.none)

newModel : Model
newModel =
    Model newSet [] ""

newRoute : String -> Route
newRoute color =
    Route color "V0"

newSet : Set
newSet =
    Set "" "" "" []

setFromJson : D.Decoder Set
setFromJson =
    D.map4 Set
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "expires" D.string)
        (D.field "colors" (D.list D.string))

voteToJson : Model -> E.Value
voteToJson model =
    E.object
        [ ("id", E.string model.set.id)
        , ("routes", E.list routeToJson model.routes)
        ]

routeToJson : Route -> E.Value
routeToJson route =
    E.object
        [ ("color", E.string route.color)
        , ("grade", E.string route.grade)
        ]

-- UPDATE

type Msg
    = ShowSet D.Value
    | SelectGrade String String
    | CastVote

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ShowSet maybeSet ->
            case D.decodeValue setFromJson maybeSet of
                Ok set ->
                    ( { model | set = set, routes = routesFromColors set.colors }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "All voting closed" }
                    , Cmd.none )

        SelectGrade color grade ->
            ( { model | routes = selectGrade model.routes color grade }
            , Cmd.none )

        CastVote ->
            ( { newModel | msg = "Thanks for your vote!" }
            , castVote (voteToJson model)
            )


routesFromColors : List String -> List Route
routesFromColors colors =
    List.map newRoute colors

selectGrade : List Route -> String -> String -> List Route
selectGrade routes color grade =
    List.map (replaceGradeIfMatch color grade) routes

replaceGradeIfMatch : String -> String -> Route -> Route
replaceGradeIfMatch color grade route =
    if route.color == color then
        Route route.color grade
    else
        route

-- VIEW

view : Model -> Html Msg
view model =
    if String.isEmpty model.msg then
        Grid.container []
            [ Grid.row []
                [ Grid.col [] [ h1 [] [ text model.set.name ] ]
                ]
            , div [] (List.map viewRoute model.routes)
            , Button.button
                [ Button.primary, Button.block
                , Button.attrs [ onClick CastVote ]
                ]
                [ text "Cast vote" ]
            ]
    else
        Grid.container []
            [ Grid.row []
                [ Grid.col []
                    [ Alert.simplePrimary [] [ text model.msg ] ]
                ]
            ]


viewRoute : Route -> Html Msg
viewRoute route =
    Card.config []
        |> Card.block []
            [ Block.text [] [ text route.color ]
            , Block.custom <|
                Select.select [ Select.onChange (SelectGrade route.color) ]
                    [ Select.item [ value "V0" ] [ text "V0" ]
                    , Select.item [ value "V1" ] [ text "V1" ]
                    ]
            ]
        |> Card.view



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    openSet ShowSet
