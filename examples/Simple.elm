module Simple exposing (main)

{-| A simple Arborist app modeling a conversation flow.
-}

import Arborist
import Arborist.Settings as Settings
import Arborist.Tree as Tree
import Browser
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Json.Encode as Encode


{-| Entry point
-}
main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| The Node data type held in each of the tree's nodes.
-}
type alias MyNode =
    { answer : String
    , question : String
    }


type alias Model =
    { arborist : Arborist.State
    , tree : Tree.Tree MyNode
    }


{-| The starting tree
-}
tree : Tree.Tree MyNode
tree =
    Tree.Node (MyNode "Q1" "A1")
        [ Tree.Node (MyNode "Q2" "A2")
            [ Tree.Node (MyNode "Q3" "A3") []
            ]
        , Tree.Node (MyNode "Q4" "A4")
            [ Tree.Node (MyNode "Q5" "A5") []
            ]
        ]


arboristSettings : List (Arborist.Setting MyNode)
arboristSettings =
    [ Settings.keyboardNavigation True
    , Settings.defaultNode (MyNode "A" "Q")
    , Settings.nodeWidth 100
    , Settings.level 80
    , Settings.gutter 20
    ]


init : Encode.Value -> ( Model, Cmd Msg )
init _ =
    ( { arborist = Arborist.init
      , tree = tree
      }
    , Cmd.none
    )


type Msg
    = Arborist (Arborist.Updater MyNode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Arborist updater ->
            let
                ( newState, newTree ) =
                    updater model.arborist model.tree
            in
            ( { model
                | arborist = newState
                , tree = newTree
              }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    Arborist.view
        [ style "border" "1px solid black"
        ]
        { state = model.arborist
        , tree = model.tree
        , nodeView = nodeView
        , settings = arboristSettings
        , toMsg = Arborist
        }


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : Arborist.NodeView MyNode Msg
nodeView _ maybeNode =
    case maybeNode of
        Just node ->
            text node.question

        Nothing ->
            text "+ add node"


subscriptions : Model -> Sub Msg
subscriptions model =
    Arborist.subscriptions arboristSettings model.arborist model.tree
        |> Sub.map
            (\( newState, newTree ) ->
                Arborist (\_ _ -> ( newState, newTree ))
            )
