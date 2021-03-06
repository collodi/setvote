port module Vote exposing (..)

import Debug

import Browser
import Html exposing (Html, text, div, h3, h5, hr)
import Html.Attributes exposing (type_, value, for, selected, disabled, hidden)
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
port castFav : E.Value -> Cmd msg

port votedRoutes : (D.Value -> msg) -> Sub msg
port favVoted : (D.Value -> msg) -> Sub msg

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
    , notVoted: Maybe (List String)
    , route : Maybe String
    , grade : Maybe String
    , fav : Maybe String
    , favVoted : Bool
    , msg : String
    }

type alias Set =
    { id : String
    , name : String
    , open : Bool
    , category : String
    , colors : List String
    }

init : () -> (Model, Cmd Msg)
init _ =
      (newModel, Cmd.none)

newModel : Model
newModel =
    Model newSet Nothing Nothing Nothing Nothing True ""

newSet : Set
newSet =
    Set "" "" False "" []

setFromJson : D.Decoder Set
setFromJson =
    D.map5 Set
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "open" D.bool)
        (D.field "category" D.string)
        (D.field "colors" (D.list D.string))

voteToJson : String -> String -> String -> E.Value
voteToJson setId route grade =
    E.object
        [ ("set_id", E.string setId)
        , ("route", E.string route)
        , ("grade", E.string grade)
        ]

favToJson : String -> String -> E.Value
favToJson setId route =
    E.object
        [ ("set_id", E.string setId)
        , ("route", E.string route)
        ]

votedRoutesFromJson : D.Decoder (List String)
votedRoutesFromJson =
    D.list D.string


-- UPDATE

type Msg
    = ShowSet D.Value
    | VotedRoutes D.Value
    | SelectRoute String
    | SelectGrade String
    | CastVote
    | SelectFav String
    | CastFav
    | FavVoted D.Value

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ShowSet maybeSet ->
            case D.decodeValue setFromJson maybeSet of
                Ok set ->
                    ( { model | set = set }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "All voting closed." }
                    , Cmd.none )

        VotedRoutes maybeVotedRoutes ->
            case D.decodeValue votedRoutesFromJson maybeVotedRoutes of
                Ok voted ->
                    ( { model | notVoted = Just (unvotedRoutes model.set.colors voted) }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "Error parsing voted routes." }
                    , Cmd.none )

        FavVoted maybeFavVoted ->
            case D.decodeValue D.bool maybeFavVoted of
                Ok voted ->
                    ( { model | favVoted = voted }
                    , Cmd.none )

                Err e ->
                    ( { model | msg = "Error parsing favorite votes." }
                    , Cmd.none )

        SelectRoute route ->
            ( { model | route = Just route }
            , Cmd.none )

        SelectGrade grade ->
            ( { model | grade = Just grade }
            , Cmd.none )

        CastVote ->
            case ( model.route, model.grade ) of
                ( Nothing, _ ) ->
                    ( { model | msg = "Choose a route!" }
                    , Cmd.none )
                ( _, Nothing ) ->
                    ( { model | msg = "Choose a grade!" }
                    , Cmd.none )
                ( Just route, Just grade ) ->
                    (
                        { model
                            | msg = "Thanks for your vote!"
                            , route = Nothing
                            , grade = Nothing
                        }
                    , castVote (voteToJson model.set.id route grade)
                    )

        SelectFav route ->
            ( { model | fav = Just route }
            , Cmd.none )

        CastFav ->
            case model.fav of
                Nothing ->
                    ( { model | msg = "Choose your favorite route!" }
                    , Cmd.none )
                Just fav ->
                    (
                        { model
                            | msg = ""
                            , fav = Nothing
                        }
                    , castFav (favToJson model.set.id fav)
                    )


unvotedRoutes : List String -> List String -> List String
unvotedRoutes routes voted =
    List.filter (not << isVoted voted) routes


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
                    , Grid.col [] [ hr [ Spacing.my1 ] [] ]
                    ]
            )
        , Grid.row [] (
            case model.notVoted of
                Nothing ->
                    [ Grid.col [ Col.attrs [ Spacing.mt3 ] ]
                        [ Alert.simplePrimary
                            []
                            [ text "Thanks for voting! There is no open vote at the time." ] ]
                    ]
                Just [] ->
                    [ Grid.col [ Col.attrs [ Spacing.mt3 ] ]
                        [ Alert.simplePrimary [] [ text "You voted for every route!" ] ]
                    ]
                Just notVoted ->
                    [ Grid.col []
                        [ Select.select
                            [ Select.onChange SelectRoute
                            , Select.attrs [ Spacing.mt3 ]
                            ]
                            ( buildOptions model.route notVoted )
                        , Select.select
                            [ Select.onChange SelectGrade
                            , Select.attrs [ Spacing.mt3 ]
                            ]
                            ( gradeChoices model.set.category
                                |> buildOptions model.grade
                            )
                        ]
                    , Grid.colBreak []
                    , Grid.col []
                        [ Button.button
                            [ Button.primary, Button.block
                            , Button.attrs [ Spacing.mt4, onClick CastVote ]
                            ]
                            [ text "Cast vote" ]
                        ]
                    , Grid.colBreak []
                    , Grid.col [ Col.attrs [ Spacing.mt3 ] ]
                        (
                            if String.isEmpty model.msg then
                                []
                            else
                                [ Alert.simplePrimary [] [ text model.msg ] ]
                        )
                    ]
            )
        , Grid.row [] [ Grid.col [] [ hr [] [] ] ]
        , Grid.row []
            (
                if model.favVoted then
                    [ Grid.col
                        [ Col.attrs [ Spacing.mt3 ] ]
                        [ Alert.simpleInfo []
                            [ text "Thanks for your favorite vote!" ]
                        ]
                    ]
                else
                    [ Grid.col [] [ text "What is your favorite route?" ]
                    , Grid.colBreak []
                    , Grid.col []
                        [ Select.select
                            [ Select.onChange SelectFav
                            , Select.attrs [ Spacing.mt3 ]
                            ]
                            ( buildOptions model.fav model.set.colors )
                        ]
                    , Grid.colBreak []
                    , Grid.col []
                        [ Button.button
                            [ Button.info, Button.block
                            , Button.attrs [ Spacing.mt4, onClick CastFav ]
                            ]
                            [ text "Vote favorite route" ]
                        ]
                    ]
            )
        ]


buildOptions: Maybe String -> List String -> List (Select.Item Msg)
buildOptions val choices =
    let
        tail =
            List.map selectChoice choices
        head =
            Select.item
                [ value "", disabled True, selected (val == Nothing), hidden True ]
                [ text "" ]
    in
        head :: tail

isVoted : List String -> String -> Bool
isVoted routes rt =
    List.member rt routes

gradeChoices : String -> List String
gradeChoices category =
    if category == "boulder" then
        [ "V0", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8" ]
    else
        [ "5.6", "5.7", "5.8", "5.9", "5.10a/b", "5.10b/c", "5.10c/d", "5.11", "5.12", "5.13" ]

selectChoice : String -> Select.Item Msg
selectChoice val =
    Select.item [ value val ] [ text val ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ openSet ShowSet
        , votedRoutes VotedRoutes
        , favVoted FavVoted
        ]
