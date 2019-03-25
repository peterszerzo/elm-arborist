module Simple exposing (main)

{-| A simple Arborist app modeling a conversation flow.
-}

import Arborist
import Arborist.Settings as Settings
import Arborist.Tree as Tree
import Browser
import Browser.Dom as Dom
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Json.Encode as Encode
import Svg
import Svg.Attributes
import Task


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


setQuestion : String -> Node -> Node
setQuestion val item =
    { item | question = val }


setAnswer : String -> Node -> Node
setAnswer val item =
    { item | answer = val }


type EditMode
    = NoEdit
    | Question
    | Answer


{-| Program model.
-}
type alias Model =
    { arborist : Arborist.State Node
    , tree : Tree.Tree Node
    , windowSize : Maybe { width : Int, height : Int }
    , editMode : EditMode

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


nodeHeight : Int
nodeHeight =
    45


arboristSettings : Model -> List (Arborist.Setting Node)
arboristSettings model =
    [ Settings.centerOffset 0 -150
    , Settings.level 100
    , Settings.gutter 40
    , Settings.nodeWidth 160
    , Settings.nodeHeight nodeHeight
    , Settings.canvasWidth (model.windowSize |> Maybe.map .width |> Maybe.withDefault 1000)
    , Settings.canvasHeight (model.windowSize |> Maybe.map .height |> Maybe.withDefault 600)
    , Settings.dragAndDrop False
    , Settings.showPlaceholderLeavesAdvanced
        (\{ node, parent, children, siblings } ->
            node.answer == "yes"
        )
    ]


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    ( { arborist = Arborist.init
      , tree = tree
      , editMode = NoEdit
      , windowSize = Nothing
      , newNode =
            { question = ""
            , answer = ""
            }
      }
    , Dom.getViewport
        |> Task.map (\v -> Resize (floor v.viewport.width) (floor v.viewport.height))
        |> Task.perform identity
    )


type Msg
    = Arborist (Arborist.State Node -> Tree.Tree Node -> ( Arborist.State Node, Tree.Tree Node ))
    | EditNewNodeQuestion String
    | EditNewNodeAnswer String
    | SetActive Node
    | DeleteActive
    | SetEditMode EditMode
    | Resize Int Int



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

        SetEditMode editMode ->
            ( { model
                | editMode = editMode
              }
            , Cmd.none
            )

        EditNewNodeAnswer val ->
            ( { model
                | newNode = setAnswer val model.newNode
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model
                | windowSize =
                    Just
                        { width = width
                        , height = height
                        }
              }
            , Cmd.none
            )



-- View


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style "margin" "auto"
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "top" "0px"
        , Html.Attributes.style "left" "0px"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "overflow" "hidden"
        ]
    <|
        [ Html.node "style" [] [ Html.text """
html, body {
  height: 100%;
}

body {
  margin: 0;
  padding: 0;
  height: 100%;
}
        """ ]
        , Element.text ""
            |> layout
                [ htmlAttribute <| Html.Attributes.style "display" "none"
                ]
        , Arborist.view
            [ Html.Attributes.style "background-color" "#fefefe"
            ]
            { state = model.arborist
            , tree = model.tree
            , nodeView = nodeView model.editMode
            , settings = arboristSettings model
            , toMsg = Arborist
            }
        ]
            ++ (Arborist.activeNode
                    { settings = arboristSettings model
                    , state = model.arborist
                    , tree = model.tree
                    }
                    |> Maybe.map
                        (\( item, { position } ) ->
                            let
                                ( x, y ) =
                                    position
                            in
                            [ column
                                [ spacing 20
                                , width fill
                                , height (px 240)
                                , Background.color (rgb255 255 255 255)
                                , largeShadow
                                , Border.rounded 4
                                , padding 20
                                ]
                                (case item of
                                    Just justItem ->
                                        [ input
                                            { onChange = \val -> SetActive { justItem | question = val }
                                            , value = justItem.question
                                            , label = Just "Question"
                                            }
                                        , input
                                            { onChange = \val -> SetActive { justItem | answer = val }
                                            , value = justItem.answer
                                            , label = Just "Answer"
                                            }
                                        , button
                                            { onPress = Just DeleteActive
                                            , label = "Delete"
                                            }
                                        ]

                                    Nothing ->
                                        [ input
                                            { onChange = EditNewNodeQuestion
                                            , value = model.newNode.question
                                            , label = Just "Question"
                                            }
                                        , input
                                            { onChange = EditNewNodeAnswer
                                            , value = model.newNode.answer
                                            , label = Just "Answer"
                                            }
                                        , button
                                            { onPress = Just (SetActive model.newNode)
                                            , label = "Add node"
                                            }
                                        ]
                                )
                                |> layoutWith
                                    { options = [ noStaticStyleSheet ]
                                    }
                                    [ htmlAttribute <| Html.Attributes.style "position" "absolute"
                                    , htmlAttribute <| Html.Attributes.style "top" "0px"
                                    , htmlAttribute <| Html.Attributes.style "left" "0px"
                                    , htmlAttribute <| Html.Attributes.style "transform" <| "translate3d(" ++ String.fromFloat (x - 210) ++ "px," ++ String.fromFloat (y + 60) ++ "px, 0px)"
                                    , htmlAttribute <| Html.Attributes.style "transition" "transform 0.3s ease-in-out"
                                    , width (px 420)
                                    , height (px 240)
                                    ]
                            ]
                        )
                    |> Maybe.withDefault []
               )


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : EditMode -> Arborist.NodeView Node Msg
nodeView editMode context maybeNode =
    maybeNode
        |> Maybe.map
            (\node ->
                el
                    ([ Just <|
                        Background.color <|
                            case context.state of
                                Arborist.Active ->
                                    rgb255 0 255 0

                                Arborist.Hovered ->
                                    rgb255 255 0 0

                                Arborist.DropTarget ->
                                    rgb255 255 0 0

                                Arborist.Normal ->
                                    rgb255 0 0 0
                     , Font.color black |> Just
                     , Border.rounded 4 |> Just
                     , smallShadow |> Just
                     , Background.color white |> Just
                     , if context.state == Arborist.Active then
                        onLeft
                            (el
                                [ width (px 8)
                                , height (px 8)
                                , moveLeft -4
                                , moveDown 10
                                , Border.rounded 4
                                , Background.color blue
                                ]
                                none
                            )
                            |> Just

                       else
                        Nothing
                     , el
                        [ width fill
                        , moveUp 20
                        , Background.color white
                        , smallShadow
                        , height (px 20)
                        , Events.onClick (SetEditMode Answer)
                        ]
                        (case ( context.state, editMode ) of
                            ( Arborist.Active, Answer ) ->
                                input
                                    { value = node.answer
                                    , onChange =
                                        \newAnswer ->
                                            SetActive
                                                { node
                                                    | answer =
                                                        newAnswer
                                                }
                                    , label = Nothing
                                    }

                            _ ->
                                el
                                    (bodyType
                                        ++ [ centerX
                                           , centerY
                                           ]
                                    )
                                <|
                                    text node.answer
                        )
                        |> above
                        |> Just
                     , padding 6 |> Just
                     , width fill |> Just
                     , height (nodeHeight |> px) |> Just
                     ]
                        |> List.filterMap identity
                    )
                    (paragraph
                        (bodyType
                            ++ [ centerX
                               , centerY
                               , Font.center
                               ]
                        )
                        [ text node.question
                        ]
                    )
            )
        |> Maybe.withDefault
            (el
                [ width fill
                , height (nodeHeight |> px)
                , Border.width 2
                , Border.dashed
                , Border.color (rgb255 200 200 200)
                ]
                (el
                    (bodyType
                        ++ [ centerX
                           , centerY
                           , Font.color (rgb255 200 200 200)
                           ]
                    )
                 <|
                    text "New child"
                )
            )
        |> layoutWith
            { options = [ noStaticStyleSheet ]
            }
            []


logo : Html.Html msg
logo =
    Svg.svg [ Svg.Attributes.viewBox "0 0 1000 1000", Svg.Attributes.fill "currentColor" ]
        [ Svg.path [ Svg.Attributes.d "M520,720l220,-220l220,220l-440,0Z" ] []
        , Svg.path [ Svg.Attributes.d "M40,720l220,-220l220,220l-440,0Z" ] []
        , Svg.path [ Svg.Attributes.d "M280,480l220,-220l220,220l-440,0Z" ] []
        ]


bodyType : List (Attribute msg)
bodyType =
    [ Font.size 14
    , Font.family
        [ Font.typeface "Source Sans Pro"
        ]
    ]


button : { onPress : Maybe msg, label : String } -> Element msg
button config =
    Input.button
        [ Background.color blue
        , Font.color white
        , Font.size 14
        , paddingXY 10 4
        , Border.rounded 2
        ]
        { onPress = config.onPress
        , label = el bodyType (text config.label)
        }


input : { value : String, onChange : String -> msg, label : Maybe String } -> Element msg
input config =
    Input.text
        [ Font.size 14
        , paddingXY 6 2
        , Font.family
            [ Font.typeface "Source Sans Pro"
            ]
        ]
        { onChange = config.onChange
        , text = config.value
        , placeholder = Nothing
        , label =
            config.label
                |> Maybe.map
                    (Element.text
                        >> Input.labelAbove []
                    )
                |> Maybe.withDefault (Input.labelHidden "Label")
        }


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


blue : Color
blue =
    rgb255 30 90 112


lighterBlue : Color
lighterBlue =
    rgb255 50 105 125


smallShadow : Attribute msg
smallShadow =
    Border.shadow
        { offset = ( 0, 2 )
        , size = 0
        , blur = 12
        , color = rgba255 0 0 0 0.1
        }


largeShadow : Attribute msg
largeShadow =
    Border.shadow
        { offset = ( 0, 3 )
        , size = 0
        , blur = 16
        , color = rgba255 0 0 0 0.2
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Arborist.subscriptions (arboristSettings model) model.arborist model.tree
            |> Sub.map
                (\( newState, newTree ) ->
                    Arborist (\_ _ -> ( newState, newTree ))
                )
        , Browser.Events.onResize Resize
        ]
