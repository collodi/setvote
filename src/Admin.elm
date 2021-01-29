port module Admin exposing (..)

import Browser
import Html exposing (Html, text, div, span, h5, hr)
import Html.Attributes exposing (value, for, style, href)
import Html.Events exposing (onInput, onClick)

import Bootstrap.Alert as Alert
import Bootstrap.Badge as Badge

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block

import Bootstrap.ListGroup as ListGroup

import Bootstrap.Form as Form
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Spacing as Spacing

import Json.Encode as E
import Json.Decode as D

port addSet : E.Value -> Cmd msg
port deleteSet : E.Value -> Cmd msg

port allSets : (D.Value -> msg) -> Sub msg
port allPolls : (D.Value -> msg) -> Sub msg
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
    { newSet : NewSet
    , showNewSet : Bool
    , sets : List Set
    , polls : List Poll
    , msg : String
    , authd : Bool
    }

type alias NewSet =
    { name : String
    , expires : String
    , newColor : String
    , category : String
    , colors : List String
    , msg : String
    }

type alias Set =
    { id : String
    , name : String
    , expires : String
    , category : String
    , colors : List String
    , showDelete : Bool
    }

type alias Poll =
    { set_id : String
    , route : String
    , fav : Int
    , grades : List String
    , counts : List Int
    }

init : () -> (Model, Cmd Msg)
init _ =
      (initModel, Cmd.none)

initModel : Model
initModel =
    Model initNewSet False [] [] "" False

initNewSet : NewSet
initNewSet =
    NewSet "" "" "" "boulder" [] ""

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

pollsFromJson : D.Decoder (List Poll)
pollsFromJson =
    D.list pollFromJson

pollFromJson : D.Decoder Poll
pollFromJson =
    D.map5 Poll
        (D.field "set_id" D.string)
        (D.field "route" D.string)
        (D.field "fav" D.int)
        (D.field "grades" (D.list D.string))
        (D.field "counts" (D.list D.int))

-- UPDATE

type Msg
    = UpdateNewSet NewSetMsg
    | ToggleNewSet
    | ToggleDelete Set
    | DeleteSet String
    | AllSets D.Value
    | Polls D.Value
    | Authd D.Value

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
            if String.isEmpty model.newSet.name then
                (
                    { model
                        | newSet = showMsgInNewSet "What's the name of this set?" model.newSet
                    }
                , Cmd.none )
            else if String.isEmpty model.newSet.expires then
                (
                    { model
                        | newSet = showMsgInNewSet "When does this set expire?" model.newSet
                    }
                , Cmd.none )
            else if List.isEmpty model.newSet.colors then
                (
                    { model
                        | newSet = showMsgInNewSet "What color are the routes?" model.newSet
                    }
                , Cmd.none )
            else
                ( { model
                    | newSet = initNewSet
                    , showNewSet = False }
                , addSet (newSetToJson model.newSet) )

        UpdateNewSet newSetMsg ->
            ( { model | newSet = updateNewSet newSetMsg model.newSet }
            , Cmd.none )

        ToggleNewSet ->
            ( { model
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

        Polls maybePolls ->
            case D.decodeValue pollsFromJson maybePolls of
                Ok polls ->
                    ( { model | polls = polls }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "Error in parsing polls" }
                    , Cmd.none )

        Authd value ->
            case D.decodeValue D.bool value of
                Ok authd_ ->
                    ( { model | authd = authd_ }
                    , Cmd.none )

                Err e ->
                    ( { model
                        | msg = "Error in parsing authd"
                        , authd = False }
                    , Cmd.none )

showMsgInNewSet : String -> NewSet -> NewSet
showMsgInNewSet msg newSet =
    { newSet | msg = msg }

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
            if String.isEmpty newSet.newColor then
                newSet
            else
                { newSet
                    | colors = newSet.newColor :: newSet.colors
                    , newColor = "" }

        DelColor color ->
            { newSet
                | colors = deleteFirst color newSet.colors
            }

        _ -> newSet

deleteFirst : String -> List String -> List String
deleteFirst target list =
    case list of
        [] ->
            []

        x :: xs ->
            if x == target then
                xs
            else
                x :: (deleteFirst target xs)



-- VIEW

view : Model -> Html Msg
view model =
    Grid.container [ Spacing.my3 ] (
        if not model.authd then
            [ Grid.row []
                [ Grid.col []
                    [ Alert.simpleWarning [] [ text "Who are you?" ]
                    , Button.linkButton
                        [ Button.primary, Button.block, Button.attrs [ href "/login.html" ] ]
                        [ text "Go To Login Page" ]
                    ]
                ]
            ]
        else if model.showNewSet then
            [ Grid.row []
                [ Grid.col []
                    [ Html.map UpdateNewSet (viewNewSet model.newSet)
                    , div [ Spacing.my2 ] []
                    , Button.button
                        [ Button.block, Button.secondary, Button.attrs [ onClick ToggleNewSet ] ]
                        [ text "Cancel" ]
                    ]
                ]
            ]
        else
            [ Grid.row []
                [ Grid.col [] [ text model.msg ]
                , Grid.colBreak []
                , Grid.col []
                    [ Button.button
                        [ Button.primary, Button.block, Button.attrs [ onClick ToggleNewSet ] ]
                        [ text "Add A Set" ]
                    ]
                , Grid.colBreak []
                , Grid.col [] (List.map (viewSet model.polls) model.sets)
                ]
            ]
        )


viewSet : List Poll -> Set -> Html Msg
viewSet polls set =
    Card.config [ Card.attrs [ Spacing.my3 ] ]
        |> Card.block []
            [ Block.titleH3 [ Spacing.mb1 ]
                [ text set.name ]
            , Block.text [ Spacing.mb3, Spacing.ml1 ]
                [ text ("expires " ++ set.expires) ]
            , Block.custom <|
                ListGroup.ul (List.map (viewRoute set polls) set.colors)
            ]
        |> Card.footer []
            [
                if set.showDelete then
                    div []
                        [ span [ Spacing.mr2 ] [ text "Really delete?" ]
                        , Button.button
                            [ Button.danger
                            , Button.attrs
                                [ Spacing.mr1, onClick (DeleteSet set.id) ]
                            ]
                            [ text "Delete" ]
                        , Button.button
                            [ Button.secondary
                            , Button.attrs [ onClick (ToggleDelete set) ]
                            ]
                            [ text "Nevermind" ]
                        ]
                else
                    Button.button
                        [ Button.danger
                        , Button.attrs [ onClick (ToggleDelete set) ]
                        ]
                        [ text "Delete Set" ]
            ]
        |> Card.view


viewRoute : Set -> List Poll -> String -> ListGroup.Item Msg
viewRoute set polls route =
    ListGroup.li [ ListGroup.attrs [ Border.none ] ] (
        case findPoll set route polls of
            Just poll ->
                [ h5 [ Display.inlineBlock ] [ text route ]
                , Badge.pillSecondary [ Spacing.ml3 ] [ text ("⭐ " ++ (String.fromInt poll.fav)) ]
                , ListGroup.ul (List.map2 viewGrade poll.grades poll.counts)
                ]

            Nothing ->
                [ h5 [ style "display" "inline-block" ] [ text route ]
                , ListGroup.ul
                    [ ListGroup.li
                        [ ListGroup.disabled, ListGroup.attrs [ Border.none ] ]
                        [ text "No votes yet" ]
                    ]
                ]

        )

findPoll : Set -> String -> List Poll -> Maybe Poll
findPoll set route polls =
    List.filter
        (\x -> x.set_id == set.id && x.route == route)
        polls
        |> List.head

viewGrade : String -> Int -> ListGroup.Item Msg
viewGrade grade count =
    ListGroup.li [ ListGroup.attrs [ Border.none ] ]
        [ h5 [ Display.inlineBlock ] [ Badge.badgeDark [] [ text grade ] ]
        , span [ Spacing.ml1 ] [ text (String.fromInt count) ]
        ]

viewNewSetColor : String -> ListGroup.Item NewSetMsg
viewNewSetColor color =
    ListGroup.li []
        [ Grid.row []
            [ Grid.col []
                [ div [ Size.h100, Spacing.py2 ] [ text color ] ]
            , Grid.col [ Col.xsAuto ]
                [ Button.button
                    [ Button.danger, Button.attrs [ onClick (DelColor color) ] ]
                    [ text "Delete" ]
                ]
            ]
        ]

viewNewSet : NewSet -> Html NewSetMsg
viewNewSet newSet =
    Grid.row []
        [ Grid.col []
            [ Form.label [ for "name" ] [ text "Name" ]
            , Input.text [ Input.id "name", Input.attrs [ value newSet.name, onInput Name ] ]
            ]
        , Grid.colBreak [ Spacing.my1 ]
        , Grid.col []
            [ Form.label [ for "close-date" ] [ text "Voting expires on" ]
            , Input.date [ Input.id "close-date", Input.attrs [ value newSet.expires, onInput CloseDate ] ]
            ]
        , Grid.colBreak [ Spacing.my2 ]
        , Grid.col []
            [ Radio.radio
                [ Radio.id "category"
                , Radio.checked (newSet.category == "boulder")
                , Radio.onClick (Category "boulder")
                ]
                "Boulder"
            ]
        , Grid.col []
            [ Radio.radio
                [ Radio.id "category"
                , Radio.checked (newSet.category == "rope")
                , Radio.onClick (Category "rope")
                ]
                "Rope"
            ]
        , Grid.colBreak [ Spacing.my2 ]
        , Grid.col []
            [ Form.label [ for "new-color" ] [ text "Add Color" ]
            , Input.text [ Input.id "new-color", Input.attrs [ value newSet.newColor, onInput NewColor ] ]
            , div [ Spacing.m1 ] []
            , Button.button
                [ Button.warning, Button.block, Button.attrs [ onClick AddColor ] ]
                [ text "Add Color" ]
            ]
        , Grid.colBreak [ Spacing.my1 ]
        , Grid.col []
            [ ListGroup.ul (List.map viewNewSetColor newSet.colors) ]
        , Grid.colBreak [ Spacing.my1 ]
        , Grid.col [] [ hr [] [] ]
        , Grid.colBreak [ Spacing.my1 ]
        , Grid.col [] (
            if String.isEmpty newSet.msg then
                []
            else
                [ Alert.simpleDanger [] [ text newSet.msg ] ]
            )
        , Grid.colBreak [ Spacing.my1 ]
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
        , allPolls Polls
        , authd Authd
        ]
