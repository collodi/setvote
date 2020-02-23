port module Admin exposing (..)

import Browser
import Html exposing (Html, text, button, div, input, span, h1)
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
import Json.Decode as D

port addSet : E.Value -> Cmd msg
port deleteSet : E.Value -> Cmd msg

port allSets : (D.Value -> msg) -> Sub msg
port allVotes : (D.Value -> msg) -> Sub msg


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
    , showNewSet : Bool
    , sets : List Set
    , votes : List Vote
    , msg : String
    }

type alias NewSet =
    { name : String
    , expires : String
    , newColor : String
    , category : String
    , colors : List String
    }

type alias Set =
    { id : String
    , name : String
    , expires : String
    , category : String
    , colors : List String
    , showDelete : Bool
    }

type alias Vote =
    { set_id : String
    , color : String
    , grade : String
    }

init : () -> (Model, Cmd Msg)
init _ =
      (initModel, Cmd.none)

initModel : Model
initModel =
    Model initNewSet False [] [] ""

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

allSetsFromJson : D.Decoder (List Set)
allSetsFromJson =
    D.list setFromJson

setFromJson : D.Decoder Set
setFromJson =
    D.map6 Set
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "expires" D.string)
        (D.field "category" D.string)
        (D.field "colors" (D.list D.string))
        (D.field "showDelete" D.bool)

allVotesFromJson : D.Decoder (List Vote)
allVotesFromJson =
    D.list voteFromJson

voteFromJson : D.Decoder Vote
voteFromJson =
    D.map3 Vote
        (D.field "set_id" D.string)
        (D.field "color" D.string)
        (D.field "grade" D.string)

-- UPDATE

type Msg
    = UpdateNewSet NewSetMsg
    | ToggleNewSet
    | ToggleDelete Set
    | DeleteSet String
    | AllSets D.Value
    | AllVotes D.Value

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

        ToggleNewSet ->
            (
                { model
                    | showNewSet = not model.showNewSet
                    , newSet = initNewSet }
            , Cmd.none )

        ToggleDelete set ->
            ( { model | sets = List.map (toggleDeleteIfMatch set) model.sets }
            , Cmd.none )

        DeleteSet id ->
            ( model
            , deleteSet (E.string id) )

        AllSets setsValue ->
            case D.decodeValue allSetsFromJson setsValue of
                Ok sets ->
                    ( { model | sets = sets }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "Error in parsing all sets" }
                    , Cmd.none )

        AllVotes votesValue ->
            case D.decodeValue allVotesFromJson votesValue of
                Ok votes ->
                    ( { model | votes = votes }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "Error in parsing all votes" }
                    , Cmd.none )

toggleDeleteIfMatch : Set -> Set -> Set
toggleDeleteIfMatch setToChange set =
    if setToChange == set then
        { set | showDelete = not set.showDelete }
    else
        set

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
        [ Grid.row []
            [ Grid.col [] [ text model.msg ]
            , Grid.colBreak []
            ,
                if model.showNewSet then
                    Grid.col []
                        [ Html.map UpdateNewSet (viewNewSet model.newSet)
                        , Button.button
                            [ Button.attrs [ onClick ToggleNewSet ] ]
                            [ text "Cancel" ]
                        ]
                else
                    Grid.col []
                        [ Button.button
                            [ Button.attrs [ onClick ToggleNewSet ] ]
                            [ text "Add A Set" ]
                        ]
            , Grid.colBreak []
            , Grid.col [] (List.map (viewSet model.votes) model.sets)
            ]
        ]

viewSet : List Vote -> Set -> Html Msg
viewSet votes set =
    div []
        [ h1 [] [ text set.name ]
        , div [] [ text ("expires " ++ set.expires) ]
        , ListGroup.ul (List.map (viewRoute set votes) set.colors)
        ,
            if set.showDelete then
                div []
                    [ div [] [ text "Really delete?" ]
                    , Button.button
                        [ Button.danger, Button.attrs [ onClick (DeleteSet set.id) ] ]
                        [ text "Delete" ]
                    , Button.button
                        [ Button.secondary, Button.attrs [ onClick (ToggleDelete set) ] ]
                        [ text "Nevermind" ]
                    ]
            else
                Button.button
                    [ Button.danger, Button.attrs [ onClick (ToggleDelete set) ] ]
                    [ text "Delete Set" ]
        ]

viewRoute : Set -> List Vote -> String -> ListGroup.Item Msg
viewRoute set votes color =
    ListGroup.li []
        [ Grid.row []
            [ Grid.col [] [ text color ]
            ]
        , Grid.row []
            (List.map
                (viewGrade set.id color votes)
                (
                    if set.category == "boulder" then
                        ["V0", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"]
                    else
                        [ "5.6", "5.7", "5.8", "5.9", "5.10a/b", "5.10b/c", "5.10c/d", "5.11", "5.12", "5.13" ]
                )
            )
        ]

viewGrade : String -> String -> List Vote -> String -> Grid.Column Msg
viewGrade set_id color votes grade =
    Grid.col []
        [ div [] [ text grade ]
        , div []
            [ text (String.fromInt (countVotes set_id color grade votes)) ]
        ]

countVotes : String -> String -> String -> List Vote -> Int
countVotes set_id color grade votes =
    List.filter (checkVoteMatch set_id color grade) votes
        |> List.length

checkVoteMatch : String -> String -> String -> Vote -> Bool
checkVoteMatch set_id color grade vote =
    vote.set_id == set_id &&
    vote.color == color &&
    vote.grade == grade

viewNewSet : NewSet -> Html NewSetMsg
viewNewSet newSet =
    div []
        [ newVoteForm newSet
        , div [ Spacing.m2 ] []
        , ListGroup.ul (List.map viewNewSetColor newSet.colors)
        ]

viewNewSetColor : String -> ListGroup.Item NewSetMsg
viewNewSetColor color =
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
    Sub.batch
        [ allSets AllSets
        , allVotes AllVotes
        ]
