module Simple exposing (main)

{-| This is a simple `Arborist` app showing a basic conversation flow
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


{-| This is the starting tree, built up recursively using the `Arborist.Tree` module
-}
tree : Tree.Tree MyNode
tree =
    Tree.Node (MyNode "How are you?" "Fine, thanks")
        [ Tree.Node (MyNode "Great. Would you like coffee?" "Absolutely") []
        ]


{-| We are ready to initialize the model
-}
init : Encode.Value -> ( Model, Cmd Msg )
init _ =
    ( { arboristState = Arborist.init
      , tree = tree
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
    , Settings.gutter 20
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
        []
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
            text node.question

        Nothing ->
            text "+ add node"


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
