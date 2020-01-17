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
import Json.Decode as Decode
import Json.Encode as Encode
import Landing.Constants as Constants
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
    , windowSize : Maybe WindowSize
    , version : String

    -- Keep track of a to-be-inserted node
    , newNode : Node.Node

    -- Features
    , dragAndDrop : Bool
    , canCreateNodes : Bool
    , keyboardNavigation : Bool
    }


type alias WindowSize =
    { width : Int
    , height : Int
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


canvasWidth : WindowSize -> Int
canvasWidth =
    .width >> (\w -> w - 120)


canvasHeight : WindowSize -> Int
canvasHeight =
    .height


mainOnlyArboristSettings : Model -> List (Arborist.Setting Node.Node)
mainOnlyArboristSettings model =
    model.windowSize
        |> Maybe.map
            (\windowSize ->
                [ Settings.centerOffset -100 -100
                , Settings.level Constants.level
                , Settings.gutter Constants.gutter
                , Settings.nodeWidth Constants.nodeWidth
                , Settings.extendConnectorsByAdvanced
                    (\context ->
                        context.node
                            |> Maybe.andThen
                                (\node ->
                                    if node.isClustered then
                                        Nothing

                                    else
                                        Just 20
                                )
                    )
                , Settings.nodeHeight Constants.nodeHeight
                , Settings.canvasWidth (canvasWidth windowSize)
                , Settings.canvasHeight (canvasHeight windowSize)
                , Settings.keyboardNavigationOutside "node-editor" model.keyboardNavigation
                , Settings.connectorStrokeWidth "2"
                , Settings.connectorStroke <| Ui.rgbToCssString Ui.greenRgb
                ]
            )
        |> Maybe.withDefault []


mainArboristSettings : Model -> List (Arborist.Setting Node.Node)
mainArboristSettings model =
    mainOnlyArboristSettings model ++ sharedArboristSettings model


minimapArboristSettings : Model -> List (Arborist.Setting Node.Node)
minimapArboristSettings model =
    Minimap.minimapOnlyArboristSettings ++ sharedArboristSettings model


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        version =
            Decode.decodeValue Decode.string flags
                |> Result.withDefault "8.2.0"
    in
    ( { arborist = Arborist.init
      , tree = tree
      , windowSize = Nothing
      , version = version
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
            ( { model
                | keyboardNavigation = keyboardNavigation
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


centerMark : Attribute msg
centerMark =
    inFront <|
        if True then
            none

        else
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
    case model.windowSize of
        Nothing ->
            column
                [ width fill
                , height fill
                ]
                [ el
                    (Ui.bodyType
                        ++ [ Font.color Ui.green
                           , centerX
                           , centerY
                           ]
                    )
                    (text "...")
                ]
                |> layout []

        Just windowSize ->
            if windowSize.width < 600 then
                column
                    [ width fill
                    , height fill
                    , Font.color Ui.green
                    , padding 20
                    ]
                    [ column
                        [ centerX
                        , centerY
                        , spacing 30
                        , moveUp 20
                        ]
                        [ el
                            [ width (px 64)
                            , height (px 64)
                            , moveDown 20
                            , centerX
                            ]
                            (html logo)
                        , paragraph (Ui.headingType ++ [ Font.center ]) [ text "elm-arborist" ]
                        , paragraph (Ui.bodyType ++ [ Font.center ]) [ text "Parameterized Tree Editor for Elm" ]
                        , row
                            [ centerX
                            , spacing 20
                            ]
                            [ link Ui.linkType
                                { url = "https://github.com/peterszerzo/elm-arborist/tree/master"
                                , label = text "GitHub"
                                }
                            , link Ui.linkType
                                { url = "http://package.elm-lang.org/packages/peterszerzo/elm-arborist/latest"
                                , label = text "v8.2.0"
                                }
                            ]
                        ]
                    ]
                    |> layout []

            else
                row
                    [ width fill
                    , height fill
                    , clip
                    ]
                    [ column
                        [ height fill
                        , spacing 20
                        , width (px 120)
                        , Background.color Ui.white
                        , Ui.smallShadow
                        ]
                        [ column
                            [ width fill
                            , paddingXY 0 10
                            ]
                            [ el
                                [ width (px 54)
                                , height (px 54)
                                , Font.color Ui.green
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
                            [ width (px 100)
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
                                (text <| "v" ++ model.version)
                            , link
                                [ centerX
                                , mouseOver
                                    [ Font.color Ui.green
                                    ]
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
                        , inFront <|
                            el
                                [ width (px Minimap.canvasWidth)
                                , height (px Minimap.canvasHeight)
                                , Border.rounded 4
                                , alignLeft
                                , alignTop
                                , moveRight 20
                                , moveDown 20
                                , clip
                                , Ui.smallShadow
                                , let
                                    dims =
                                        centerAreaDims
                                            { canvasWidth = canvasWidth windowSize
                                            , canvasHeight = canvasHeight windowSize
                                            , nodeWidth = Constants.nodeWidth
                                            , nodeHeight = Constants.nodeHeight
                                            , level = Constants.level
                                            , gutter = Constants.gutter
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
                        , inFront <|
                            if model.dragAndDrop then
                                none

                            else
                                Arborist.activeNode
                                    { settings = mainArboristSettings model
                                    , state = model.arborist
                                    , tree = model.tree
                                    }
                                    |> Maybe.map (activeNodePopup model.newNode)
                                    |> Maybe.withDefault none
                        ]
                      <|
                        [ if model.windowSize == Nothing then
                            none

                          else
                            Arborist.view
                                []
                                { state = model.arborist
                                , tree = model.tree
                                , nodeView = nodeView
                                , settings = mainArboristSettings model
                                , toMsg = Arborist
                                }
                                |> html
                        ]
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
    -> Element Msg
activeNodePopup newNode ( item, _ ) =
    let
        ( children, controls ) =
            case item of
                Just justItem ->
                    ( [ Ui.input
                            { onChange = \val -> SetActive { justItem | answer = val }
                            , value = justItem.answer
                            , label = Just "Reach this node if answer is:"
                            }
                      , Ui.input
                            { onChange = \val -> SetActive { justItem | question = val }
                            , value = justItem.question
                            , label = Just "New question"
                            }
                      ]
                    , [ Ui.button
                            []
                            { onPress = Just DeleteActive
                            , label = "Delete node"
                            , isError = True
                            }
                      , Ui.button
                            [ Background.color Ui.green
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
                            { onChange = EditNewNodeAnswer
                            , value = newNode.answer
                            , label = Just "Reach this node if answer is:"
                            }
                      , Ui.input
                            { onChange = EditNewNodeQuestion
                            , value = newNode.question
                            , label = Just "New question"
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
    column
        [ spacing 20
        , Background.color (rgb255 255 255 255)
        , Ui.smallShadow
        , alignRight
        , padding 20
        , width (px 400)
        , height fill
        , htmlAttribute <| Html.Attributes.id "node-editor"
        , el
            [ width (px 0)
            , height (px 0)
            , moveRight (210 - 14)
            , moveUp 2
            , Ui.htmlStyle "border-left" "12px solid transparent"
            , Ui.htmlStyle "border-right" "12px solid transparent"
            , Ui.htmlStyle "border-bottom" <| "12px solid " ++ Ui.rgbToCssString Ui.greenRgb
            ]
            none
            |> above
        ]
        (children
            ++ [ row
                    [ centerX
                    , alignTop
                    , spacing 10
                    ]
                    controls
               ]
        )


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : Arborist.NodeView Node.Node Msg
nodeView context maybeNode =
    case maybeNode of
        Just node ->
            column
                ([ width (px Constants.nodeWidth)
                 , height (px Constants.nodeHeight)
                 , Background.color Ui.green
                 , Border.rounded 4
                 , padding 10
                 , pointer
                 , mouseOver
                    [ Background.color Ui.lighterGreen
                    ]
                 , el
                    (Ui.bodyType
                        ++ [ Font.color Ui.green
                           , centerX
                           , centerY
                           ]
                    )
                    (text node.answer)
                    |> el
                        [ width (px 80)
                        , height (px 28)
                        , moveUp 12
                        , moveRight (toFloat Constants.nodeWidth / 2 - 40)
                        , Border.width 2
                        , Border.rounded 4
                        , Border.color Ui.green
                        , Background.color Ui.white
                        ]
                    |> above
                 ]
                    ++ (if context.state == Arborist.DropTarget then
                            [ Background.color Ui.lighterGreen
                            ]

                        else
                            []
                       )
                    ++ (if context.state == Arborist.Active then
                            [ el
                                [ width (px <| Constants.nodeWidth + 8)
                                , height (px <| Constants.nodeHeight + 8)
                                , Background.color Ui.green
                                , alpha 0.4
                                , Border.rounded 4
                                , moveUp 4
                                , moveLeft 4
                                ]
                                none
                                |> behindContent
                            ]

                        else if node.isClustered then
                            [ el
                                [ width (px Constants.nodeWidth)
                                , height (px Constants.nodeHeight)
                                , Background.color Ui.green
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
                    (Ui.bodyType
                        ++ [ centerX
                           , centerY
                           , width fill
                           , Font.center
                           , spacing 5
                           , Font.color Ui.white
                           ]
                    )
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
                [ width (px Constants.nodeWidth)
                , height (px Constants.nodeHeight)
                , Border.rounded 4
                , Border.width 2
                , Border.color Ui.green
                , Font.color Ui.green
                , pointer
                , mouseOver
                    [ Background.color Ui.faintGreen
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
