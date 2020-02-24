port module Vote exposing (..)

import Browser
import Html exposing (Html, text, div, h3, h5, hr)
import Html.Attributes exposing (type_, value, for)
import Html.Events exposing (onInput, onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.Alert as Alert

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block

import Bootstrap.ListGroup as ListGroup

import Bootstrap.Form.Select as Select

import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Border as Border

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
    , category : String
    , colors : List String
    }

init : () -> (Model, Cmd Msg)
init _ =
      (newModel, Cmd.none)

newModel : Model
newModel =
    Model newSet [] ""

newRoute : String -> String -> Route
newRoute category color =
    if category == "boulder" then
        Route color "V0"
    else
        Route color "5.6"

newSet : Set
newSet =
    Set "" "" "" "" []

setFromJson : D.Decoder Set
setFromJson =
    D.map5 Set
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "expires" D.string)
        (D.field "category" D.string)
        (D.field "colors" (D.list D.string))

voteToJson : Model -> E.Value
voteToJson model =
    E.object
        [ ("set_id", E.string model.set.id)
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
                    (
                        { model
                            | set = set
                            , routes = routesFromColors set.category set.colors }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "All voting closed." }
                    , Cmd.none )

        SelectGrade color grade ->
            ( { model | routes = selectGrade model.routes color grade }
            , Cmd.none )

        CastVote ->
            ( { model | msg = "Thanks for your vote!" }
            , castVote (voteToJson model)
            )


routesFromColors : String -> List String -> List Route
routesFromColors category colors =
    List.map (newRoute category) colors

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
    Grid.container []
        [ Grid.row
            [ Row.attrs [ Spacing.mt3 ] ]
            (
                if String.isEmpty model.set.name then
                    []
                else
                    [ Grid.col []
                        [ h3 [] [ text model.set.name ] ]
                    , Grid.colBreak []
                    , Grid.col [ Col.attrs [ Spacing.mb3, Spacing.ml1 ] ]
                        [ text ("expires " ++ model.set.expires) ]
                    , Grid.colBreak []
                    , Grid.col [] [ hr [ Spacing.my1 ] [] ]
                    ]
            )
        , Grid.row [] (
            if String.isEmpty model.msg then
                [ Grid.col []
                    [ ListGroup.ul
                        (List.map (viewRoute model.set.category) model.routes)
                    , Button.button
                        [ Button.primary, Button.block
                        , Button.attrs [ Spacing.mt4, onClick CastVote ]
                        ]
                        [ text "Cast vote" ]
                    ]
                ]
            else
                [ Grid.col [ Col.attrs [ Spacing.mt3 ] ]
                    [ Alert.simplePrimary [] [ text model.msg ] ]
                ]
            )
        ]


viewRoute : String -> Route -> ListGroup.Item Msg
viewRoute category route =
    ListGroup.li
        [ ListGroup.attrs [ Border.none ] ]
        [ h5 [ Spacing.mb2 ] [ text route.color ]
        , Select.select
            [ Select.onChange (SelectGrade route.color) ]
            ( gradeChoices category )
        ]

gradeChoices : String -> List (Select.Item Msg)
gradeChoices category =
    if category == "boulder" then
        List.map gradeChoice
            [ "V0", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8" ]
    else
        List.map gradeChoice
            [ "5.6", "5.7", "5.8", "5.9", "5.10a/b", "5.10b/c", "5.10c/d", "5.11", "5.12", "5.13" ]

gradeChoice : String -> Select.Item Msg
gradeChoice grade =
    Select.item [ value grade ] [ text grade ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    openSet ShowSet
