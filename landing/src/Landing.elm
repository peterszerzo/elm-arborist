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
    , isClustered : Bool
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
    , windowSize : Maybe { width : Int, height : Int }

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
    Tree.Node
        { answer = "Hello"
        , question = "Do you like trees?"
        , isClustered = False
        }
        [ Tree.Node
            { answer = "Yes"
            , question = "How much?"
            , isClustered = True
            }
            [ Tree.Node
                { answer = "A lot!"
                , question = "Where were you all my life?"
                , isClustered = False
                }
                []
            ]
        , Tree.Node
            { answer = "No"
            , question = "Seriously?"
            , isClustered = False
            }
            [ Tree.Node
                { answer = "Yes"
                , question = "How about rollercoasters?"
                , isClustered = False
                }
                []
            ]
        ]


nodeHeight : Int
nodeHeight =
    45


nodeWidth : Int
nodeWidth =
    160


arboristSettings : Model -> List (Arborist.Setting Node)
arboristSettings model =
    [ Settings.centerOffset 0 -150
    , Settings.level 100
    , Settings.gutter 40
    , Settings.nodeWidth nodeWidth
    , Settings.nodeHeight nodeHeight
    , Settings.canvasWidth (model.windowSize |> Maybe.map .width |> Maybe.withDefault 1000)
    , Settings.canvasHeight (model.windowSize |> Maybe.map .height |> Maybe.withDefault 600)
    , Settings.dragAndDrop model.dragAndDrop
    , Settings.keyboardNavigation model.keyboardNavigation
    , Settings.connectorStrokeWidth "2"
    , Settings.connectorStroke <| rgbToCssString blueRgb
    , Settings.isNodeClustered .isClustered
    , Settings.defaultNode
        { question = "New question?"
        , answer = "Answer"
        , isClustered = False
        }
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
            , isClustered = False
            }

      -- Arborist settings
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
    | SetDragAndDrop Bool
    | SetCanCreateNodes Bool
    | SetKeyboardNavigation Bool
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
            , spacing 20
            , width (px 100)
            ]
            [ column
                [ width fill
                , paddingXY 0 10
                ]
                [ el
                    [ width (px 48)
                    , height (px 48)
                    , Font.color blue
                    , centerX
                    , centerY
                    ]
                    (html logo)
                , el
                    (bodyType
                        ++ [ Font.color black
                           , centerX
                           , centerY
                           , moveUp 6
                           ]
                    )
                    (text "elm-arborist")
                ]
            , column
                [ width (px 80)
                , centerX
                , spacing 6
                , padding 8
                , Background.color (rgb255 245 245 245)
                , Border.rounded 4
                ]
                [ el
                    (bodyType
                        ++ [ Font.color black
                           , centerX
                           ]
                    )
                    (text "v7.1.0")
                , link
                    [ centerX
                    ]
                    { url = "https://github.com/peterszerzo/elm-arborist/tree/master"
                    , label =
                        el
                            (bodyType
                                ++ [ Font.underline
                                   ]
                            )
                            (text "GitHub")
                    }
                ]
            , column
                [ padding 10
                , spacing 20
                , width fill
                ]
                [ switch [ centerX ]
                    { checked = model.dragAndDrop
                    , onChange = SetDragAndDrop
                    , label = "Drag and drop"
                    }
                , switch [ centerX ]
                    { checked = model.canCreateNodes
                    , onChange = SetCanCreateNodes
                    , label = "Create"
                    }
                , switch [ centerX ]
                    { checked = model.keyboardNavigation
                    , onChange = SetKeyboardNavigation
                    , label = "Keyboard"
                    }
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
                , Background.color white
                , Border.widthEach
                    { top = 0
                    , bottom = 0
                    , left = 0
                    , right = 1
                    }
                , Border.color (rgb255 200 200 200)
                , width (px 100)
                ]
        , Arborist.view
            [ Html.Attributes.style "background-color" "#FFFFFF"
            ]
            { state = model.arborist
            , tree = model.tree
            , nodeView = nodeView
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

        ( children, controls ) =
            case item of
                Just justItem ->
                    ( [ input
                            { onChange = \val -> SetActive { justItem | question = val }
                            , value = justItem.question
                            , label = Just "Question"
                            }
                      , input
                            { onChange = \val -> SetActive { justItem | answer = val }
                            , value = justItem.answer
                            , label = Just "Answer"
                            }
                      ]
                    , [ button
                            []
                            { onPress = Just DeleteActive
                            , label = "Delete node"
                            , isError = True
                            }
                      , button
                            [ Background.color blue
                            ]
                            { onPress =
                                Just
                                    (SetActive
                                        { justItem
                                            | isClustered =
                                                not justItem.isClustered
                                        }
                                    )
                            , label =
                                if justItem.isClustered then
                                    "Expand cluster"

                                else
                                    "Cluster"
                            , isError = False
                            }
                      ]
                    )

                Nothing ->
                    ( [ input
                            { onChange = EditNewNodeQuestion
                            , value = newNode.question
                            , label = Just "Question"
                            }
                      , input
                            { onChange = EditNewNodeAnswer
                            , value = newNode.answer
                            , label = Just "Answer"
                            }
                      ]
                    , [ button []
                            { onPress = Just (SetActive newNode)
                            , label = "Add node"
                            , isError = False
                            }
                      ]
                    )
    in
    [ column
        [ spacing 20
        , width fill
        , height (px 240)
        , Background.color (rgb255 255 255 255)
        , largeShadow
        , Border.rounded 4
        , padding 20
        , row
            [ moveUp 60
            , height (px 60)
            , alignRight
            , Background.color (rgb255 245 245 245)
            , width fill
            ]
            [ row
                [ centerX
                , centerY
                , spacing 10
                ]
                controls
            ]
            |> below
        ]
        children
        |> layoutWith
            { options = [ noStaticStyleSheet ]
            }
            [ elementHtmlStyle "position" "absolute"
            , elementHtmlStyle "top" "0px"
            , elementHtmlStyle "left" "0px"
            , elementHtmlStyle "transform" <| "translate3d(" ++ String.fromFloat (x - 210) ++ "px," ++ String.fromFloat (y + 55) ++ "px, 0px)"
            , elementHtmlStyle "transition" "transform 0.3s ease-in-out"
            , width (px 420)
            , height (px 240)
            ]
    ]


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : Arborist.NodeView Node Msg
nodeView context maybeNode =
    case maybeNode of
        Just node ->
            column
                ([ width (px nodeWidth)
                 , height (px nodeHeight)
                 , Background.color blue
                 , Border.rounded 4
                 , padding 10
                 , pointer
                 , mouseOver
                    [ Background.color lighterBlue
                    ]
                 , el
                    (bodyType
                        ++ [ Font.color blue
                           , centerX
                           , centerY
                           ]
                    )
                    (text node.answer)
                    |> el
                        [ width (px 80)
                        , height (px 24)
                        , moveUp 12
                        , moveRight 42
                        , Border.width 2
                        , Border.rounded 4
                        , Border.color blue
                        , Background.color white
                        ]
                    |> above
                 ]
                    ++ (if node.isClustered then
                            [ el
                                [ width (px nodeWidth)
                                , height (px nodeHeight)
                                , Background.color blue
                                , alpha 0.4
                                , Border.rounded 4
                                , moveUp 4
                                , moveRight 4
                                ]
                                none
                                |> behindContent
                            ]

                        else
                            []
                       )
                )
                [ paragraph
                    [ centerX
                    , centerY
                    , width fill
                    , Font.center
                    , spacing 0
                    , Font.size 12
                    , Font.color white
                    ]
                    [ text <|
                        if node.isClustered then
                            "[ Cluster ]"

                        else
                            node.question
                    ]
                ]
                |> layoutWith
                    { options = [ noStaticStyleSheet ]
                    }
                    []

        Nothing ->
            column
                [ width (px 160)
                , height (px nodeHeight)
                , Border.rounded 4
                , Border.width 2
                , Border.color blue
                , Font.color blue
                , pointer
                , mouseOver
                    [ Background.color faintBlue
                    ]
                ]
                [ el
                    [ centerX
                    , centerY
                    , Font.size 28
                    ]
                  <|
                    text "+"
                ]
                |> layoutWith
                    { options = [ noStaticStyleSheet ]
                    }
                    []


logo : Html.Html msg
logo =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 1000 1000"
        , Svg.Attributes.fill "currentColor"
        ]
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


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : String
        , isError : Bool
        }
    -> Element msg
button attrs config =
    Input.button
        (bodyType
            ++ [ Font.color white
               , paddingXY 10 8
               , Border.rounded 4
               ]
            ++ (if config.isError then
                    [ Background.color red
                    ]

                else
                    [ Background.color blue
                    , focused
                        []
                    ]
               )
            ++ attrs
        )
        { onPress = config.onPress
        , label = el bodyType (text config.label)
        }


input :
    { value : String
    , onChange : String -> msg
    , label : Maybe String
    }
    -> Element msg
input config =
    Input.text
        [ Font.size 16
        , paddingXY 10 6
        , Font.family
            [ Font.typeface "Source Sans Pro"
            ]
        , focused
            [ Border.shadow
                { size = 3
                , blur = 0
                , offset = ( 0, 0 )
                , color = faintBlue
                }
            , Border.color blue
            ]
        ]
        { onChange = config.onChange
        , text = config.value
        , placeholder = Nothing
        , label =
            config.label
                |> Maybe.map
                    (text
                        >> el
                            [ htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                            , Font.size 12
                            ]
                        >> Input.labelAbove
                            [ paddingEach
                                { top = 0
                                , bottom = 4
                                , left = 10
                                , right = 0
                                }
                            ]
                    )
                |> Maybe.withDefault (Input.labelHidden "Label")
        }


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


blueRgb : ( Int, Int, Int )
blueRgb =
    ( 50, 57, 91 )


blue : Color
blue =
    rgbToColor blueRgb


lighterBlue : Color
lighterBlue =
    rgb255 68 75 105


red : Color
red =
    rgb255 220 40 30


faintBlue : Color
faintBlue =
    rgb255 199 201 210


rgbToColor : ( Int, Int, Int ) -> Color
rgbToColor ( r, g, b ) =
    rgb255 r g b


rgbToCssString : ( Int, Int, Int ) -> String
rgbToCssString ( r, g, b ) =
    "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"


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
        { offset = ( 0, 2 )
        , size = 0
        , blur = 20
        , color = rgba255 0 0 0 0.25
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
                    [ width (px 80)
                    , Font.center
                    ]
                <|
                    el
                        ([ width (px 40)
                         , height (px 18)
                         , padding 3
                         , Border.width 1
                         , Border.rounded 18
                         , Border.color blue
                         , centerX
                         ]
                            ++ (if checked then
                                    [ Background.color blue
                                    ]

                                else
                                    []
                               )
                        )
                    <|
                        el
                            ([ width (px 12)
                             , height (px 12)
                             , Font.center
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
                                        [ Background.color white
                                        ]

                                    else
                                        [ Background.color blue
                                        ]
                                   )
                            )
                            none
        , label =
            el
                (smallType
                    ++ [ Font.color black
                       , Font.center
                       ]
                )
                (text config.label)
                |> Input.labelAbove
                    [ centerX
                    ]
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
