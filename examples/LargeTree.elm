module LargeTree exposing (main)

{-| This is a simple `Arborist` app showing a basic conversation flow
-}

import Arborist
import Arborist.Settings as Settings
import Arborist.Tree as Tree
import Browser
import Html exposing (text)
import Html.Attributes
import Json.Encode as Encode
import List as List


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


{-| The Node data type held in each of the tree's nodes
-}
type alias MyNode =
    { answer : String
    , question : String
    }


{-| The application model
-}
type alias Model =
    { arboristState : Arborist.State
    , tree : Tree.Tree MyNode
    }


fromMyNodeList : List MyNode -> Tree.Tree MyNode
fromMyNodeList myNodeList =
    case myNodeList of
        [] ->
            Tree.Node (MyNode "Q" "A") []

        x :: [] ->
            Tree.Node x []

        x :: y :: xs ->
            Tree.Node x
                [ Tree.Node y []
                , fromMyNodeList xs
                ]


largeTree : Tree.Tree MyNode
largeTree =
    List.range 1 60
        |> List.map String.fromInt
        |> List.map (\index -> MyNode ("Q" ++ index) ("A" ++ index))
        |> fromMyNodeList



{- #abeeshake This largeTree is being fed as tree to be displayed. -}


{-| We are ready to initialize the model
-}
init : Encode.Value -> ( Model, Cmd Msg )
init _ =
    ( { arboristState = Arborist.init
      , tree = largeTree
      }
    , Cmd.none
    )


{-| Some basic settings for the tree
-}
arboristSettings : List (Arborist.Setting MyNode)
arboristSettings =
    [ Settings.keyboardNavigation True
    , Settings.defaultNode (MyNode "What question?" "But what an answer")
    , Settings.nodeWidth 100
    , Settings.level 80
    , Settings.gutter 60
    ]


{-| Arborist needs a single message, passing an `Updater`
-}
type Msg
    = Arborist (Arborist.Updater MyNode)


{-| The `update` method will use the updater to figure out the new tree and the new internal state
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Arborist updater ->
            let
                ( newState, newTree ) =
                    updater model.arboristState model.tree
            in
            ( { model
                | arboristState = newState
                , tree = newTree
              }
            , Cmd.none
            )


{-| Everything is prepared for the view (almost!)
-}
view : Model -> Html.Html Msg
view model =
    Arborist.view
        [ Html.Attributes.style "border" "1px solid #DEDEDE"
        ]
        { state = model.arboristState
        , tree = model.tree

        -- This method describes how a node should render based on its content
        , nodeView = nodeView
        , settings = arboristSettings
        , toMsg = Arborist
        }


{-| As you see above, it is enough to specify how a single node looks to render a tree with a full layout
-}
nodeView : Arborist.NodeView MyNode Msg
nodeView _ maybeNode =
    case maybeNode of
        Just node ->
            Html.div
                [ Html.Attributes.style "border" "1px solid #DEDEDE"
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "background-color" "#FFFFFF"
                , Html.Attributes.style "cursor" "pointer"
                ]
                [ text node.question ]

        Nothing ->
            text "+ add Newnode"


{-| Subscriptions are not mandatory, but they take care of important convenience features like keyboard navigation
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Arborist.subscriptions
        arboristSettings
        model.arboristState
        model.tree
        |> Sub.map
            (\( newState, newTree ) ->
                Arborist (\_ _ -> ( newState, newTree ))
            )
