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
import Element.Font as Font
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Landing.Minimap as Minimap
import Landing.Node as Node
import Landing.Ui as Ui
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


{-| Program model.
-}
type alias Model =
    { arborist : Arborist.State
    , tree : Tree.Tree Node.Node
    , windowSize : Maybe { width : Int, height : Int }

    -- Keep track of a to-be-inserted node
    , newNode : Node.Node

    -- Features
    , dragAndDrop : Bool
    , canCreateNodes : Bool
    , keyboardNavigation : Bool
    }


{-| The starting tree.
-}
tree : Tree.Tree Node.Node
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


level : Int
level =
    100


gutter : Int
gutter =
    40


sharedArboristSettings : Model -> List (Arborist.Setting Node.Node)
sharedArboristSettings model =
    [ Settings.dragAndDrop model.dragAndDrop
    , Settings.isNodeClustered .isClustered
    , Settings.defaultNode
        { question = "New question?"
        , answer = "Answer"
        , isClustered = False
        }
    , Settings.showPlaceholderLeaves model.canCreateNodes
    ]


canvasWidth : Model -> Int
canvasWidth model =
    model.windowSize
        |> Maybe.map (.width >> (\w -> w - 100))
        |> Maybe.withDefault 1000


canvasHeight : Model -> Int
canvasHeight model =
    model.windowSize
        |> Maybe.map .height
        |> Maybe.withDefault 600


mainOnlyArboristSettings : Model -> List (Arborist.Setting Node.Node)
mainOnlyArboristSettings model =
    [ Settings.centerOffset 0 -150
    , Settings.level level
    , Settings.gutter gutter
    , Settings.nodeWidth nodeWidth
    , Settings.nodeHeight nodeHeight
    , Settings.canvasWidth (canvasWidth model)
    , Settings.canvasHeight (canvasHeight model)
    , Settings.keyboardNavigation model.keyboardNavigation
    , Settings.connectorStrokeWidth "2"
    , Settings.connectorStroke <| Ui.rgbToCssString Ui.blueRgb
    ]


mainArboristSettings : Model -> List (Arborist.Setting Node.Node)
mainArboristSettings model =
    mainOnlyArboristSettings model ++ sharedArboristSettings model


minimapArboristSettings : Model -> List (Arborist.Setting Node.Node)
minimapArboristSettings model =
    Minimap.minimapOnlyArboristSettings ++ sharedArboristSettings model


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
    = Arborist (Arborist.State -> Tree.Tree Node.Node -> ( Arborist.State, Tree.Tree Node.Node ))
    | ArboristMinimap (Arborist.State -> Tree.Tree Node.Node -> ( Arborist.State, Tree.Tree Node.Node ))
    | EditNewNodeQuestion String
    | EditNewNodeAnswer String
    | SetActive Node.Node
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

        ArboristMinimap updater ->
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
                | newNode = Node.setQuestion val model.newNode
              }
            , Cmd.none
            )

        EditNewNodeAnswer val ->
            ( { model
                | newNode = Node.setAnswer val model.newNode
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


centerMark : Attribute msg
centerMark =
    inFront <|
        el
            [ width (px 4)
            , height (px 4)
            , Background.color (rgb 1 0 0)
            , centerX
            , centerY
            ]
            none


centerArea : Int -> Int -> Attribute msg
centerArea w h =
    inFront <|
        el
            [ width (px w)
            , height (px h)
            , Background.color (rgba255 0 0 0 0.05)
            , centerX
            , centerY
            , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
            ]
            none


centerAreaDims :
    { canvasWidth : Int
    , canvasHeight : Int
    , nodeWidth : Int
    , nodeHeight : Int
    , level : Int
    , gutter : Int
    , minimapNodeWidth : Int
    , minimapNodeHeight : Int
    , minimapLevel : Int
    , minimapGutter : Int
    }
    -> { width : Int, height : Int }
centerAreaDims config =
    { width =
        toFloat config.canvasWidth
            * (toFloat config.minimapGutter + toFloat config.minimapNodeWidth)
            / (toFloat config.gutter + toFloat config.nodeWidth)
            |> floor
    , height =
        toFloat config.canvasHeight
            * (toFloat config.minimapLevel + toFloat config.minimapNodeHeight)
            / (toFloat config.level + toFloat config.nodeHeight)
            |> floor
    }


view : Model -> Html.Html Msg
view model =
    row
        [ width fill
        , height fill
        , clip
        ]
        [ column
            [ height fill
            , spacing 20
            , width (px 100)
            , Background.color Ui.white
            , Border.color (rgb255 200 200 200)
            , Border.widthEach
                { top = 0
                , bottom = 0
                , left = 0
                , right = 1
                }
            ]
            [ column
                [ width fill
                , paddingXY 0 10
                ]
                [ el
                    [ width (px 48)
                    , height (px 48)
                    , Font.color Ui.blue
                    , centerX
                    , centerY
                    ]
                    (html logo)
                , el
                    (Ui.bodyType
                        ++ [ Font.color Ui.black
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
                    (Ui.bodyType
                        ++ [ Font.color Ui.black
                           , centerX
                           ]
                    )
                    (text "v8.0.4")
                , link
                    [ centerX
                    ]
                    { url = "https://github.com/peterszerzo/elm-arborist/tree/master"
                    , label =
                        el
                            (Ui.bodyType
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
                [ Ui.switch [ centerX ]
                    { checked = model.dragAndDrop
                    , onChange = SetDragAndDrop
                    , label = "Drag and drop"
                    }
                , Ui.switch [ centerX ]
                    { checked = model.canCreateNodes
                    , onChange = SetCanCreateNodes
                    , label = "Create"
                    }
                , Ui.switch [ centerX ]
                    { checked = model.keyboardNavigation
                    , onChange = SetKeyboardNavigation
                    , label = "Keyboard"
                    }
                ]
            ]
        , row
            [ centerMark
            , onRight <|
                el
                    [ width (px Minimap.canvasWidth)
                    , height (px Minimap.canvasHeight)
                    , Border.rounded 4
                    , moveLeft
                        (Minimap.canvasWidth
                            + 20
                            |> toFloat
                        )
                    , moveDown 20
                    , clip
                    , Border.width 1
                    , Border.color (rgb255 200 200 200)
                    , let
                        dims =
                            centerAreaDims
                                { canvasWidth = canvasWidth model
                                , canvasHeight = canvasHeight model
                                , nodeWidth = nodeWidth
                                , nodeHeight = nodeHeight
                                , level = level
                                , gutter = gutter
                                , minimapNodeWidth = Minimap.nodeSize
                                , minimapNodeHeight = Minimap.nodeSize
                                , minimapLevel = Minimap.level
                                , minimapGutter = Minimap.gutter
                                }
                      in
                      centerArea dims.width dims.height
                    , centerMark
                    ]
                    (Arborist.view
                        [ Html.Attributes.style "background-color" "#FFFFFF"
                        ]
                        { state = model.arborist
                        , tree = model.tree
                        , nodeView = Minimap.nodeView
                        , settings = minimapArboristSettings model
                        , toMsg = ArboristMinimap
                        }
                        |> html
                    )
            ]
          <|
            [ if model.windowSize == Nothing then
                none

              else
                Arborist.view
                    [ Html.Attributes.style "background-color" "#FFFFFF"
                    ]
                    { state = model.arborist
                    , tree = model.tree
                    , nodeView = nodeView
                    , settings = mainArboristSettings model
                    , toMsg = Arborist
                    }
                    |> html
            ]
                ++ (if model.dragAndDrop then
                        []

                    else
                        Arborist.activeNode
                            { settings = mainArboristSettings model
                            , state = model.arborist
                            , tree = model.tree
                            }
                            |> Maybe.map (activeNodePopup model.newNode)
                            |> Maybe.withDefault []
                            |> List.map html
                   )
        ]
        |> layout []


activeNodePopup :
    Node.Node
    ->
        ( Maybe Node.Node
        , { context : Arborist.Context Node.Node
          , position : ( Float, Float )
          }
        )
    -> List (Html.Html Msg)
activeNodePopup newNode ( item, { position } ) =
    let
        ( x, y ) =
            position

        ( children, controls ) =
            case item of
                Just justItem ->
                    ( [ Ui.input
                            { onChange = \val -> SetActive { justItem | question = val }
                            , value = justItem.question
                            , label = Just "Question"
                            }
                      , Ui.input
                            { onChange = \val -> SetActive { justItem | answer = val }
                            , value = justItem.answer
                            , label = Just "Answer"
                            }
                      ]
                    , [ Ui.button
                            []
                            { onPress = Just DeleteActive
                            , label = "Delete node"
                            , isError = True
                            }
                      , Ui.button
                            [ Background.color Ui.blue
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
                    ( [ Ui.input
                            { onChange = EditNewNodeQuestion
                            , value = newNode.question
                            , label = Just "Question"
                            }
                      , Ui.input
                            { onChange = EditNewNodeAnswer
                            , value = newNode.answer
                            , label = Just "Answer"
                            }
                      ]
                    , [ Ui.button []
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
        , Ui.largeShadow
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
            , elementHtmlStyle "transform" <| "translate3d(" ++ String.fromFloat (x - 210) ++ "px," ++ String.fromFloat (y + 35) ++ "px, 0px)"
            , elementHtmlStyle "transition" "transform 0.3s ease-in-out"
            , width (px 420)
            , height (px 240)
            ]
    ]


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : Arborist.NodeView Node.Node Msg
nodeView context maybeNode =
    case maybeNode of
        Just node ->
            column
                ([ width (px nodeWidth)
                 , height (px nodeHeight)
                 , Background.color Ui.blue
                 , Border.rounded 4
                 , padding 10
                 , pointer
                 , mouseOver
                    [ Background.color Ui.lighterBlue
                    ]
                 , el
                    (Ui.bodyType
                        ++ [ Font.color Ui.blue
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
                        , Border.color Ui.blue
                        , Background.color Ui.white
                        ]
                    |> above
                 ]
                    ++ (if node.isClustered then
                            [ el
                                [ width (px nodeWidth)
                                , height (px nodeHeight)
                                , Background.color Ui.blue
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
                    , Font.color Ui.white
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
                , Border.color Ui.blue
                , Font.color Ui.blue
                , pointer
                , mouseOver
                    [ Background.color Ui.faintBlue
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Arborist.subscriptions (mainArboristSettings model) model.arborist model.tree
            |> Sub.map
                (\( newState, newTree ) ->
                    Arborist (\_ _ -> ( newState, newTree ))
                )
        , Browser.Events.onResize Resize
        , onCtrlPlus "d" (SetDragAndDrop (not model.dragAndDrop))
        , onCtrlPlus "c" (SetCanCreateNodes (not model.canCreateNodes))
        , onCtrlPlus "k" (SetKeyboardNavigation (not model.keyboardNavigation))
        ]


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
