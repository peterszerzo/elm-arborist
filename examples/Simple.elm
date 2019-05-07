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
type alias Node =
    { question : String
    , answer : String
    }


{-| Program model.
-}
type alias Model =
    { arborist : Arborist.State Node
    , tree : Tree.Tree Node
    }


{-| The starting tree.
-}
tree : Tree.Tree Node
tree =
    Tree.Node
        { answer = "Hello"
        , question = "Do you like trees?"
        }
        [ Tree.Node
            { answer = "Yes"
            , question = "How much?"
            }
            [ Tree.Node
                { answer = "A lot!"
                , question = "Where were you all my life?"
                }
                []
            ]
        , Tree.Node
            { answer = "No"
            , question = "Seriously?"
            }
            [ Tree.Node
                { answer = "Yes"
                , question = "How about rollercoasters?"
                }
                []
            ]
        ]


nodeHeight : Int
nodeHeight =
    60


nodeWidth : Int
nodeWidth =
    160


arboristSettings : List (Arborist.Setting Node)
arboristSettings =
    [ Settings.centerOffset 0 -150
    , Settings.level 100
    , Settings.gutter 40
    , Settings.keyboardNavigation True
    , Settings.nodeWidth nodeWidth
    , Settings.nodeHeight nodeHeight
    , Settings.canvasWidth 800
    , Settings.canvasHeight 480
    , Settings.connectorStrokeWidth "2"
    , Settings.defaultNode
        { question = "New question?"
        , answer = "Answer"
        }
    , Settings.showPlaceholderLeaves True
    ]


init : Encode.Value -> ( Model, Cmd Msg )
init _ =
    ( { arborist = Arborist.init
      , tree = tree
      }
    , Cmd.none
    )


type Msg
    = Arborist (Arborist.State Node -> Tree.Tree Node -> ( Arborist.State Node, Tree.Tree Node ))



-- Update


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

        SetActive newNode ->
            let
                ( newArborist, newTree ) =
                    Arborist.setActiveNodeWithChildren
                        { node = newNode
                        , childrenOverride = Nothing
                        }
                        model.arborist
                        model.tree
            in
            ( { model
                | tree = newTree
                , arborist = newArborist
              }
            , Cmd.none
            )

        DeleteActive ->
            ( Arborist.deleteActiveNode model.arborist model.tree
                |> (\( state, tree_ ) ->
                        { model
                            | arborist =
                                state
                            , tree = tree_
                        }
                   )
            , Cmd.none
            )



-- View


view : Model -> Html.Html Msg
view model =
    Arborist.view
        [ Html.Attributes.style "background-color" "#FFFFFF"
        ]
        { state = model.arborist
        , tree = model.tree
        , nodeView = nodeView
        , settings = arboristSettings
        , toMsg = Arborist
        }


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : Arborist.NodeView Node Msg
nodeView _ maybeNode =
    case maybeNode of
        Just node ->
            div
                [ style "width" <| String.fromInt nodeWidth ++ "px"
                , style "height" <| String.fromInt nodeHeight ++ "px"
                ]
                [ text node.question
                ]

        Nothing ->
            text "nothing"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Arborist.subscriptions arboristSettings model.arborist model.tree
            |> Sub.map
                (\( newState, newTree ) ->
                    Arborist (\_ _ -> ( newState, newTree ))
                )
        ]
