module Simple exposing (main)

{-| A simple Arborist app modeling a conversation flow.
-}

import Arborist
import Arborist.Settings as Settings
import Arborist.Tree as Tree
import Browser
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html, a, button, div, h1, h2, h3, input, label, map, node, p, text)
import Html.Attributes exposing (class, href, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode


{-| The Node data type held in each of the tree's nodes.
-}
type alias Node =
    { question : String
    , answer : String
    }


setQuestion : String -> Node -> Node
setQuestion val item =
    { item | question = val }


setAnswer : String -> Node -> Node
setAnswer val item =
    { item | answer = val }


{-| Program model.
-}
type alias Model =
    { arborist : Arborist.State Node
    , tree : Tree.Tree Node

    -- Keep track of a to-be-inserted node
    , newNode : Node
    }


{-| The starting tree.
-}
tree : Tree.Tree Node
tree =
    Tree.Node { answer = "", question = "Do you like trees?" }
        [ Tree.Node { answer = "yes", question = "How much?" }
            [ Tree.Node { answer = "A lot!", question = "Where were you all my life?" } []
            ]
        , Tree.Node { answer = "No.", question = "Seriously?" }
            [ Tree.Node { answer = "Yes", question = "How about rollercoasters?" } []
            ]
        ]


arboristSettings : List (Arborist.Setting Node)
arboristSettings =
    [ Settings.centerOffset 0 -150
    , Settings.nodeHeight 45
    , Settings.level 100
    , Settings.nodeWidth 160
    , Settings.sturdyMode False
    , Settings.canvasWidth 1000
    , Settings.canvasHeight 600
    , Settings.dragAndDrop True
    , Settings.showPlaceholderLeavesAdvanced
        (\{ node, parent, children, siblings } ->
            node.answer == "yes"
        )
    ]


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    ( { arborist = Arborist.init
      , tree = tree
      , newNode =
            { question = ""
            , answer = ""
            }
      }
    , Cmd.none
    )


{-| Program message
-}
type Msg
    = Arborist (Arborist.State Node -> Tree.Tree Node -> ( Arborist.State Node, Tree.Tree Node ))
    | EditNewNodeQuestion String
    | EditNewNodeAnswer String
    | SetActive Node
    | DeleteActive



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
            ( { model
                | tree =
                    Arborist.setActiveNodeWithChildren
                        { node = newNode
                        , childrenOverride = Nothing
                        }
                        model.arborist
                        model.tree
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

        EditNewNodeQuestion val ->
            ( { model
                | newNode = setQuestion val model.newNode
              }
            , Cmd.none
            )

        EditNewNodeAnswer val ->
            ( { model
                | newNode = setAnswer val model.newNode
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div
        [ style "margin" "auto"
        , style "position" "absolute"
        , style "top" "0px"
        , style "left" "0px"
        ]
    <|
        [ Element.text ""
            |> layout []
        , Arborist.view
            []
            { state = model.arborist
            , tree = model.tree
            , nodeView = nodeView
            , settings = arboristSettings
            , toMsg = Arborist
            }
        ]
            ++ (Arborist.activeNode
                    { settings = arboristSettings
                    , state = model.arborist
                    , tree = model.tree
                    }
                    |> Maybe.map
                        (\( item, { position } ) ->
                            let
                                ( x, y ) =
                                    position
                            in
                            [ div
                                [ style "position" "absolute"
                                , style "left" <| String.fromFloat x ++ "px"
                                , style "top" <| String.fromFloat y ++ "px"
                                ]
                                (case item of
                                    Just justItem ->
                                        [ Input.text []
                                            { onChange = \val -> SetActive { justItem | question = val }
                                            , text = justItem.question
                                            , placeholder = Nothing
                                            , label = Element.text "Question" |> Input.labelAbove []
                                            }
                                            |> layoutWith
                                                { options = [ noStaticStyleSheet ]
                                                }
                                                []
                                        , Input.text []
                                            { onChange = \val -> SetActive { justItem | answer = val }
                                            , text = justItem.answer
                                            , placeholder = Nothing
                                            , label = Element.text "Answer" |> Input.labelAbove []
                                            }
                                            |> layoutWith
                                                { options = [ noStaticStyleSheet ]
                                                }
                                                []
                                        , Input.button []
                                            { onPress = Just DeleteActive
                                            , label = Element.text "Delete"
                                            }
                                            |> layoutWith
                                                { options = [ noStaticStyleSheet ]
                                                }
                                                []
                                        ]

                                    Nothing ->
                                        [ label []
                                            [ text "Question"
                                            , input
                                                [ value model.newNode.question
                                                , onInput EditNewNodeQuestion
                                                ]
                                                []
                                            ]
                                        , label []
                                            [ text "Answer"
                                            , input [ value model.newNode.answer, onInput EditNewNodeAnswer ] []
                                            ]
                                        , button
                                            [ onClick (SetActive model.newNode)
                                            ]
                                            [ text "Add node" ]
                                        ]
                                )
                            ]
                        )
                    |> Maybe.withDefault []
               )


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : Arborist.NodeView Node
nodeView context maybeNode =
    maybeNode
        |> Maybe.map
            (\node ->
                div
                    ([ ( "background-color"
                       , case context.state of
                            Arborist.Active ->
                                "green"

                            Arborist.Hovered ->
                                "blue"

                            Arborist.DropTarget ->
                                "blue"

                            Arborist.Normal ->
                                "black"
                       )
                     , ( "color", "white" )
                     ]
                        |> List.map (\( property, value ) -> style property value)
                    )
                    [ text node.question
                    ]
            )
        |> Maybe.withDefault
            (el
                ([ width fill
                 , height fill
                 ]
                    ++ ((case context.state of
                            Arborist.Active ->
                                [ ( "color", "white" )
                                , ( "border", "0" )
                                ]

                            Arborist.DropTarget ->
                                [ ( "border", "0" )
                                , ( "color", "white" )
                                ]

                            _ ->
                                [ ( "background-color", "transparent" )
                                , ( "border", "1px dashed #CECECE" )
                                , ( "color", "#898989" )
                                ]
                        )
                            |> List.map (\( property, value ) -> style property value |> htmlAttribute)
                       )
                )
                (Element.text "New child")
                |> layout
                    [ width fill
                    , height fill
                    ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Arborist.subscriptions arboristSettings model.arborist model.tree
        |> Sub.map
            (\( newState, newTree ) ->
                Arborist (\_ _ -> ( newState, newTree ))
            )


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
