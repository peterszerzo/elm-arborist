module Landing exposing (main)

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
import Html.Events
import Json.Decode as Decode
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
    , test1 : Bool
    , test2 : Bool

    -- Keep track of a to-be-inserted node
    , newNode : Node

    -- Features
    , dragAndDrop : Bool
    , canCreateNodes : Bool
    , keyboardNavigation : Bool
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
    , Settings.dragAndDrop model.dragAndDrop
    , Settings.keyboardNavigation model.keyboardNavigation
    , Settings.connectorStrokeWidth "2"
    , Settings.connectorStroke "#CECECE"
    , Settings.showPlaceholderLeavesAdvanced
        (\{ node, parent, children, siblings } ->
            model.canCreateNodes
        )
    ]


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    ( { arborist = Arborist.init
      , tree = tree
      , windowSize = Nothing
      , newNode =
            { question = ""
            , answer = ""
            }
      , test1 = False
      , test2 = False

      -- Arborist settings
      , editMode = NoEdit
      , dragAndDrop = False
      , canCreateNodes = True
      , keyboardNavigation = True
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
    | SetDragAndDrop Bool
    | SetCanCreateNodes Bool
    | SetKeyboardNavigation Bool
    | Resize Int Int
    | Test1
    | Test2



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Test1 ->
            ( { model
                | test1 = True
                , test2 = False
              }
            , Cmd.none
            )

        Test2 ->
            ( { model
                | test1 = False
                , test2 = True
              }
            , Cmd.none
            )

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

        SetDragAndDrop dragAndDrop ->
            ( { model | dragAndDrop = dragAndDrop }
            , Cmd.none
            )

        SetCanCreateNodes canCreateNodes ->
            ( { model | canCreateNodes = canCreateNodes }
            , Cmd.none
            )

        SetKeyboardNavigation keyboardNavigation ->
            ( { model | keyboardNavigation = keyboardNavigation }
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


elementHtmlStyle : String -> String -> Attribute msg
elementHtmlStyle prop val =
    htmlAttribute <| Html.Attributes.style prop val


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
        , Html.Events.on "someevent" (Decode.succeed (SetEditMode Answer))
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
        , column
            [ height fill
            ]
            [ column
                [ width (px 100)
                , height (px 100)
                ]
                [ el
                    [ width (px 48)
                    , height (px 48)
                    , Font.color white
                    , centerX
                    , centerY
                    ]
                    (html logo)
                , el
                    (bodyType
                        ++ [ Font.color white
                           , centerX
                           , centerY
                           , moveUp 6
                           ]
                    )
                    (text "elm-arborist")
                ]
            , column
                [ padding 10
                , spacing 20
                ]
                [ switch []
                    { checked = model.dragAndDrop
                    , onChange = SetDragAndDrop
                    , label = "Drag and drop"
                    }
                , switch []
                    { checked = model.canCreateNodes
                    , onChange = SetCanCreateNodes
                    , label = "Create"
                    }
                , switch []
                    { checked = model.keyboardNavigation
                    , onChange = SetKeyboardNavigation
                    , label = "Keyboard"
                    }
                ]
            , column
                [ alignBottom
                , paddingXY 10 20
                ]
                [ el
                    (bodyType
                        ++ [ Font.color white
                           ]
                    )
                    (text "v7.0.0")
                ]
            ]
            |> layoutWith
                { options = [ noStaticStyleSheet ]
                }
                [ elementHtmlStyle "position" "fixed"
                , elementHtmlStyle "top" "0px"
                , elementHtmlStyle "left" "0px"
                , elementHtmlStyle "bottom" "0px"
                , elementHtmlStyle "z-index" "100"
                , Background.color dark
                , width (px 100)
                ]
        , Arborist.view
            [ Html.Attributes.style "background-color" "#EFEFEF"
            ]
            { state = model.arborist
            , tree = model.tree
            , nodeView =
                nodeView
                    { test1 = model.test1
                    , test2 = model.test2
                    }
            , settings = arboristSettings model
            , toMsg = Arborist
            }
        ]
            ++ (if model.dragAndDrop then
                    []

                else
                    Arborist.activeNode
                        { settings = arboristSettings model
                        , state = model.arborist
                        , tree = model.tree
                        }
                        |> Maybe.map (activeNodePopup model.newNode)
                        |> Maybe.withDefault []
               )


activeNodePopup :
    Node
    -> ( Maybe Node, { context : Arborist.Context Node, position : ( Float, Float ) } )
    -> List (Html.Html Msg)
activeNodePopup newNode ( item, { position } ) =
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
                    , value = newNode.question
                    , label = Just "Question"
                    }
                , input
                    { onChange = EditNewNodeAnswer
                    , value = newNode.answer
                    , label = Just "Answer"
                    }
                , button
                    { onPress = Just (SetActive newNode)
                    , label = "Add node"
                    }
                ]
        )
        |> layoutWith
            { options = [ noStaticStyleSheet ]
            }
            [ elementHtmlStyle "position" "absolute"
            , elementHtmlStyle "top" "0px"
            , elementHtmlStyle "left" "0px"
            , elementHtmlStyle "transform" <| "translate3d(" ++ String.fromFloat (x - 210) ++ "px," ++ String.fromFloat (y + 60) ++ "px, 0px)"
            , elementHtmlStyle "transition" "transform 0.3s ease-in-out"
            , width (px 420)
            , height (px 240)
            ]
    ]


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : { test1 : Bool, test2 : Bool } -> Arborist.NodeView Node Msg
nodeView { test1, test2 } context maybeNode =
    Html.div
        [ Html.Attributes.style "width" "160px"
        , Html.Attributes.style "height" "45px"
        , Html.Attributes.style "background-color" "#343434"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        ]
        [ if context.state == Arborist.Active && test1 then
            Html.span
                [ Html.Attributes.style "background-color" "#FFFFFF"
                ]
                [ Html.text "test1" ]

          else
            Html.div
                [ Html.Attributes.style "width" "30px"
                , Html.Attributes.style "height" "30px"
                , Html.Attributes.style "background-color" "#9933EE"
                , Html.Events.onClick Test1
                ]
                []
        , if context.state == Arborist.Active && test2 then
            Html.span
                [ Html.Attributes.style "background-color" "#FFFFFF"
                ]
                [ Html.text "test2" ]

          else
            Html.div
                [ Html.Attributes.style "width" "30px"
                , Html.Attributes.style "height" "30px"
                , Html.Attributes.style "background-color" "#3399EE"
                , Html.Events.onClick Test2
                ]
                []
        ]


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


smallType : List (Attribute msg)
smallType =
    [ Font.size 11
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


dark : Color
dark =
    rgb255 25 35 50


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
        , onCtrlPlus "d" (SetDragAndDrop (not model.dragAndDrop))
        , onCtrlPlus "c" (SetCanCreateNodes (not model.canCreateNodes))
        , onCtrlPlus "k" (SetKeyboardNavigation (not model.keyboardNavigation))
        ]


customClicker : String -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
customClicker eventName attrs =
    Html.node "custom-clicker"
        (attrs
            ++ [ Html.Attributes.attribute "eventname" eventName
               ]
        )


switch :
    List (Attribute msg)
    ->
        { label : String
        , checked : Bool
        , onChange : Bool -> msg
        }
    -> Element msg
switch attrs config =
    Input.checkbox attrs
        { checked = config.checked
        , onChange = config.onChange
        , icon =
            \checked ->
                el
                    ([ width (px 40)
                     , height (px 18)
                     , padding 3
                     , Border.width 1
                     , Border.rounded 18
                     , Border.color white
                     ]
                        ++ (if checked then
                                [ Background.color white
                                ]

                            else
                                []
                           )
                    )
                <|
                    el
                        ([ width (px 12)
                         , height (px 12)
                         , moveUp 1
                         , moveRight
                            (if checked then
                                20

                             else
                                0
                            )
                         , centerY
                         , Border.rounded 14
                         ]
                            ++ (if checked then
                                    [ Background.color dark
                                    ]

                                else
                                    [ Background.color white
                                    ]
                               )
                        )
                        none
        , label =
            el
                (smallType
                    ++ [ Font.color white
                       ]
                )
                (text config.label)
                |> Input.labelAbove []
        }


onCtrlPlus : String -> msg -> Sub msg
onCtrlPlus targetKey message =
    Browser.Events.onKeyPress
        (Decode.map2
            (\key ctrlKey ->
                if ctrlKey && key == targetKey then
                    Just ()

                else
                    Nothing
            )
            (Decode.field "key" Decode.string)
            (Decode.field "ctrlKey" Decode.bool)
            |> Decode.andThen
                (\res ->
                    case res of
                        Just _ ->
                            Decode.succeed message

                        Nothing ->
                            Decode.fail ""
                )
        )