module Arborist
    exposing
        ( Model
        , Msg(SetActive, DeleteActive)
        , init
        , update
        , view
        , tree
        , isNew
        , setNew
        , active
        )

import Dict
import Json.Decode as Decode
import Html exposing (Html, Attribute, node, div, text, p, h1, h3, label, input, button)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput, on, onWithOptions)
import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, stroke, strokeWidth)
import Html.Events exposing (onClick)
import Data.Tree as Tree exposing (TreeNodePath)
import Data.ComputedTree as ComputedTree
import MultiDrag as MultiDrag
import Utils
import Views.Styles as Styles
import Views.NodeConnectors
import Arborist.Config as Config


-- Model


type Model item
    = Model
        { computedTree : ComputedTree.ComputedTree item
        , prevComputedTree : ComputedTree.ComputedTree item
        , new : Maybe TreeNodePath
        , active : Maybe TreeNodePath
        , isDragging : Bool
        , focus : Maybe TreeNodePath
        , drag : MultiDrag.Drag (Maybe TreeNodePath)
        , displayRoot : Maybe TreeNodePath
        , panOffset : ( Float, Float )
        }


init : Tree.Tree item -> Model item
init tree =
    Model
        { computedTree = ComputedTree.init tree
        , prevComputedTree = ComputedTree.init tree
        , active = Nothing
        , new = Nothing
        , isDragging = False
        , focus = Nothing
        , drag = (MultiDrag.init)
        , displayRoot = Nothing
        , panOffset = ( 0, 0 )
        }


active : Model item -> Maybe item
active (Model { active, computedTree }) =
    let
        tree =
            ComputedTree.tree computedTree
    in
        active
            |> Maybe.andThen
                (\active ->
                    Tree.find active tree
                        |> Maybe.andThen
                            (\subtree ->
                                case subtree of
                                    Tree.Empty ->
                                        Nothing

                                    Tree.Node item children ->
                                        Just item
                            )
                )


setNew : item -> Model item -> Model item
setNew item (Model model) =
    Model
        { model
            | computedTree =
                model.new
                    |> Maybe.map
                        (\new ->
                            Tree.insert new (Just item) (ComputedTree.tree model.computedTree) |> ComputedTree.init
                        )
                    |> Maybe.withDefault model.computedTree
            , prevComputedTree = model.computedTree
        }


tree : Model item -> Tree.Tree item
tree (Model { computedTree }) =
    ComputedTree.tree computedTree


isNew : Model item -> Bool
isNew (Model { new }) =
    new /= Nothing



-- Msg


type Msg item
    = Deactivate
    | SetActive item
    | DeleteActive
    | SetNew TreeNodePath
    | SetFocus (Maybe TreeNodePath)
    | MouseMove Float Float
    | NodeMouseDown Bool TreeNodePath Float Float
    | NodeMouseUp Float Float
    | CanvasMouseDown Float Float
    | CanvasMouseUp Float Float
    | NoOp



-- Update


update : Config.Config item (Msg item) -> Msg item -> Model item -> Model item
update config msg (Model model) =
    case msg of
        SetActive newItem ->
            Model
                { model
                    | computedTree =
                        model.active
                            |> Maybe.map (\active -> Tree.update active newItem (ComputedTree.tree model.computedTree) |> ComputedTree.init)
                            |> Maybe.withDefault model.computedTree
                    , prevComputedTree = model.computedTree
                }

        DeleteActive ->
            Model
                { model
                    | computedTree =
                        model.active
                            |> Maybe.map (\active -> Tree.delete active (ComputedTree.tree model.computedTree) |> ComputedTree.init)
                            |> Maybe.withDefault model.computedTree
                    , prevComputedTree = model.computedTree
                }

        SetFocus focus ->
            Model model

        SetNew nodePath ->
            Model
                { model
                    | new = Just nodePath
                    , active = Nothing
                }

        Deactivate ->
            Model { model | active = Nothing, new = Nothing }

        MouseMove xm ym ->
            Model
                { model
                    | drag =
                        MultiDrag.move xm ym model.drag
                    , isDragging =
                        if MultiDrag.state model.drag /= Nothing then
                            True
                        else
                            False
                }

        NodeMouseDown isPlaceholder path x y ->
            Model
                { model
                    | drag =
                        if isPlaceholder then
                            model.drag
                        else
                            MultiDrag.start (Just path) x y
                    , new =
                        if isPlaceholder then
                            Just (List.take (List.length path - 1) path)
                        else
                            Nothing
                }

        NodeMouseUp x y ->
            let
                active =
                    MultiDrag.state model.drag
                        |> Maybe.andThen
                            (\( path, ( offsetX, offsetY ) ) ->
                                case path of
                                    Just path ->
                                        if (abs offsetX + abs offsetY > 20) then
                                            Nothing
                                        else
                                            Just path

                                    Nothing ->
                                        Nothing
                            )

                flat =
                    ComputedTree.flat model.computedTree

                tree =
                    ComputedTree.tree model.computedTree

                newTree =
                    MultiDrag.state model.drag
                        |> Maybe.map
                            (\( path, dragOffset ) ->
                                path
                                    |> Maybe.map
                                        (\path ->
                                            getDropTarget config path dragOffset model.computedTree
                                                |> Maybe.map
                                                    (\dropTargetPath ->
                                                        flat
                                                            |> List.filter (\( path, item ) -> path == dropTargetPath)
                                                            |> List.head
                                                            |> Maybe.andThen
                                                                (\( path, item ) ->
                                                                    case item of
                                                                        Just item ->
                                                                            Just ( path, item )

                                                                        Nothing ->
                                                                            Nothing
                                                                )
                                                            |> Maybe.map (\_ -> Tree.swap path dropTargetPath tree)
                                                            |> Maybe.withDefault
                                                                -- If the drop target is a placeholder, first add an Empty node in the original tree
                                                                -- so the swap method actually finds a node.
                                                                (Tree.insert (List.take (List.length dropTargetPath - 1) dropTargetPath) Nothing tree
                                                                    |> Tree.swap path dropTargetPath
                                                                    |> Tree.removeEmpties
                                                                )
                                                    )
                                                |> Maybe.withDefault tree
                                        )
                                    |> Maybe.withDefault tree
                            )
                        |> Maybe.withDefault tree
            in
                Model
                    { model
                        | drag =
                            MultiDrag.init
                        , computedTree = ComputedTree.init newTree
                        , prevComputedTree = model.computedTree
                        , active = active
                        , isDragging = False
                    }

        CanvasMouseDown x y ->
            Model
                { model
                    | drag = MultiDrag.start Nothing x y
                }

        CanvasMouseUp x y ->
            Model
                { model
                    | drag = MultiDrag.init
                    , panOffset =
                        MultiDrag.state model.drag
                            |> Maybe.map
                                (\( draggedNode, offset ) ->
                                    let
                                        ( panOffsetX, panOffsetY ) =
                                            model.panOffset

                                        ( offsetX, offsetY ) =
                                            if draggedNode == Nothing then
                                                offset
                                            else
                                                ( 0, 0 )
                                    in
                                        ( panOffsetX + offsetX, panOffsetY + offsetY )
                                )
                            |> Maybe.withDefault model.panOffset
                }

        NoOp ->
            Model model


type alias NodeGeometry =
    { center : ( Float, Float )
    , childCenters : List ( Float, Float )
    }


getDropTarget : Config.Config item (Msg item) -> TreeNodePath -> ( Float, Float ) -> ComputedTree.ComputedTree item -> Maybe TreeNodePath
getDropTarget config path ( dragX, dragY ) computedTree =
    let
        flat =
            ComputedTree.flat computedTree

        layout =
            ComputedTree.layout computedTree

        ( x0, y0 ) =
            nodeGeometry config path layout
                |> Maybe.map .center
                |> Maybe.withDefault ( 0, 0 )

        ( x, y ) =
            ( x0 + dragX
            , y0 + dragY
            )
    in
        flat
            |> List.filter
                (\( path_, _ ) ->
                    let
                        ( xo, yo ) =
                            nodeGeometry config path_ layout
                                |> Maybe.map .center
                                |> Maybe.withDefault ( 0, 0 )
                    in
                        path /= path_ && (abs (x - xo) < config.layout.width) && (abs (y - yo) < config.layout.height)
                )
            |> List.filter
                (\( dropTargetPath, _ ) ->
                    -- Node that are in a descendant or ancestor relationship with the dragged node cannot be drop targets
                    not (Utils.startsWith path dropTargetPath || Utils.startsWith dropTargetPath path)
                )
            |> List.head
            |> Maybe.map Tuple.first


nodeGeometry : Config.Config item (Msg item) -> List Int -> Tree.Layout -> Maybe NodeGeometry
nodeGeometry config path layout =
    layout
        |> Dict.get path
        |> Maybe.map
            (\{ center, childCenters } ->
                let
                    ( centerX, centerY ) =
                        center
                in
                    { center =
                        ( centerX * (config.layout.width + config.layout.horizontalGap)
                        , centerY * (config.layout.height + config.layout.verticalGap)
                        )
                    , childCenters =
                        List.map
                            (\center ->
                                ( center * (config.layout.width + config.layout.horizontalGap)
                                , (centerY + 1) * (config.layout.height + config.layout.verticalGap)
                                )
                            )
                            childCenters
                    }
            )


view : Config.Config item (Msg item) -> List (Attribute (Msg item)) -> Model item -> Html (Msg item)
view config attrs (Model model) =
    let
        flatTree =
            ComputedTree.flat model.computedTree

        layout =
            ComputedTree.layout model.computedTree

        nodeBaseStyle =
            Styles.nodeBase config.layout.width config.layout.height

        coordStyle =
            Styles.coordinate config.layout.width

        dragState =
            MultiDrag.state model.drag

        isNodeDragging =
            dragState
                |> Maybe.map Tuple.first
                |> Maybe.map ((/=) Nothing)
                |> Maybe.withDefault False

        ( canvasDragOffset, isCanvasDragging ) =
            dragState
                |> Maybe.map
                    (\( path, offset ) ->
                        if path == Nothing then
                            ( offset, True )
                        else
                            ( ( 0, 0 ), False )
                    )
                |> Maybe.withDefault ( ( 0, 0 ), False )

        ( canvasTotalDragOffsetX, canvasTotalDragOffsetY ) =
            Utils.addFloatTuples canvasDragOffset model.panOffset
    in
        div
            ([ on "mousemove"
                (Decode.map2 MouseMove
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                )
             , on "mousedown"
                (Decode.map2 CanvasMouseDown
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                )
             , on "mouseup"
                (Decode.map2 CanvasMouseUp
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                )
             , on "mouseleave"
                (Decode.map2 CanvasMouseUp
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                )
             , style
                [ ( "overflow", "hidden" )
                , ( "cursor"
                  , if isCanvasDragging then
                        "move"
                    else
                        "auto"
                  )
                ]
             , onClick Deactivate
             ]
                ++ attrs
            )
            [ div
                [ style
                    [ ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "position", "relative" )
                    , ( "transform", "translate3d(" ++ (toString canvasTotalDragOffsetX) ++ "px, " ++ (toString canvasTotalDragOffsetY) ++ "px, 0)" )
                    ]
                ]
              <|
                (List.map
                    (\( path, item ) ->
                        nodeGeometry config path layout
                            |> Maybe.map
                                (\{ center, childCenters } ->
                                    let
                                        ( x, y ) =
                                            center

                                        ( isDragged, ( xDrag, yDrag ), draggedPath, isDropTarget ) =
                                            if model.isDragging then
                                                dragState
                                                    |> Maybe.map
                                                        (\( draggedPath, offset ) ->
                                                            case draggedPath of
                                                                Just draggedPath ->
                                                                    let
                                                                        isDropTarget =
                                                                            getDropTarget config draggedPath offset model.computedTree
                                                                                |> Maybe.map (\dropTargetPath -> Utils.startsWith dropTargetPath path)
                                                                                |> Maybe.withDefault False
                                                                    in
                                                                        if Utils.startsWith draggedPath path then
                                                                            ( True, offset, Just draggedPath, isDropTarget )
                                                                        else
                                                                            ( False, ( 0, 0 ), Just draggedPath, isDropTarget )

                                                                Nothing ->
                                                                    ( False, ( 0, 0 ), Nothing, False )
                                                        )
                                                    |> Maybe.withDefault ( False, ( 0, 0 ), Nothing, False )
                                            else
                                                ( False, ( 0, 0 ), Nothing, False )

                                        isActive =
                                            model.active == Just path || isDragged

                                        xWithDrag =
                                            x + xDrag

                                        yWithDrag =
                                            y + yDrag

                                        itemViewContext =
                                            { parent = Nothing
                                            , siblings = []
                                            , position = ( x, y )
                                            , isActive = isActive
                                            , isDropTarget = isDropTarget
                                            }
                                    in
                                        [ div
                                            [ style <|
                                                nodeBaseStyle
                                                    ++ (coordStyle xWithDrag yWithDrag)
                                                    ++ (if isDragged then
                                                            [ ( "z-index", "100" )
                                                            , ( "cursor", "move" )
                                                            ]
                                                        else
                                                            []
                                                       )
                                            , Utils.onClickStopPropagation NoOp
                                            , onWithOptions "mousedown"
                                                { stopPropagation = True
                                                , preventDefault = False
                                                }
                                                (Decode.map2 (NodeMouseDown (item == Nothing) path)
                                                    (Decode.field "screenX" Decode.float)
                                                    (Decode.field "screenY" Decode.float)
                                                )
                                            , onWithOptions "mouseup"
                                                { stopPropagation = True
                                                , preventDefault = False
                                                }
                                                (Decode.map2 NodeMouseUp
                                                    (Decode.field "screenX" Decode.float)
                                                    (Decode.field "screenY" Decode.float)
                                                )
                                            ]
                                            [ item
                                                |> Maybe.map (config.view itemViewContext)
                                                |> Maybe.withDefault (config.placeholderView itemViewContext)
                                            ]
                                        ]
                                            ++ (if isDragged then
                                                    [ div
                                                        [ style <|
                                                            nodeBaseStyle
                                                                ++ Styles.dragShadowNode
                                                                ++ (coordStyle x y)
                                                        ]
                                                        []
                                                    ]
                                                else
                                                    []
                                               )
                                            ++ [ Views.NodeConnectors.view config.layout ( xDrag, yDrag ) center childCenters
                                               ]
                                )
                            |> Maybe.withDefault []
                    )
                    flatTree
                    |> List.foldl (++) []
                )
            ]
