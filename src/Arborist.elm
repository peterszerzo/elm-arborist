module Arborist
    exposing
        ( Model
        , Msg
        , UpdateConfig
        , subscriptions
        , init
        , update
        , updateWith
        , view
        , tree
        , activeNode
        , setActiveNode
        , deleteActiveNode
        )

{-| Drag-and-drop interface to edit, dissect and-rearrange tree structures with arbitrary data sitting in their nodes.


# The editor module

@docs Model, Msg, init, update, updateWith, UpdateConfig, view, subscriptions


# Tree query and manipulations

@docs tree, activeNode, setActiveNode, deleteActiveNode

-}

import Dict
import AnimationFrame
import Json.Decode as Decode
import Html exposing (Html, Attribute, node, div, text, p, h1, h3, label, input, button)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput, on, onWithOptions)
import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, stroke, strokeWidth)
import Html.Events exposing (onClick)
import Utils.Tree as Tree exposing (TreeNodePath)
import Arborist.Tree
import Data.ComputedTree as ComputedTree
import MultiDrag as MultiDrag
import Utils
import Views.Styles as Styles
import Views.NodeConnectors
import Arborist.Config as Config
import Messages


type ActiveNode
    = None
    | ExistingNode TreeNodePath
    | NewNode TreeNodePath


type alias NodeGeometry =
    { center : ( Float, Float )
    , childCenters : List ( Float, Float )
    }


{-| Model, used for type annotation.
-}
type Model item
    = Model
        { computedTree : ComputedTree.ComputedTree item
        , prevComputedTree : ComputedTree.ComputedTree item
        , active : Maybe TreeNodePath
        , hovered : Maybe TreeNodePath
        , isDragging : Bool
        , isReceivingAnimationFrames : Bool
        , focus : Maybe TreeNodePath
        , drag : MultiDrag.Drag (Maybe TreeNodePath)
        , displayRoot : Maybe TreeNodePath
        , panOffset : ( Float, Float )
        , targetPanOffset : Maybe ( Float, Float )
        }


{-| Initialize model from a [tree](/Arborist-Tree).
-}
init : Arborist.Tree.Tree item -> Model item
init tree =
    Model
        { computedTree = ComputedTree.init tree
        , prevComputedTree = ComputedTree.init tree
        , active = Nothing
        , hovered = Nothing
        , isDragging = False
        , isReceivingAnimationFrames = False
        , focus = Nothing
        , drag = (MultiDrag.init)
        , displayRoot = Nothing
        , panOffset = ( 0, 0 )
        , targetPanOffset = Nothing
        }


{-| Returns the current active node. Returns a tuple of `Maybe item` (as the node maybe a placeholder for a new node), as well as its coordinates on the screen. Use these coordinates to position an active node-related pop-up (see example).
-}
activeNode : Config.Config item -> Model item -> Maybe ( Maybe item, ( Float, Float ) )
activeNode config (Model { active, computedTree, panOffset, drag }) =
    active
        |> Maybe.map
            (\active ->
                let
                    layout =
                        ComputedTree.layout computedTree

                    geo =
                        nodeGeometry config active layout
                            |> Maybe.map .center
                            |> Maybe.withDefault ( 0, 0 )

                    dragOffset =
                        MultiDrag.state drag
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault ( 0, 0 )
                in
                    ( ComputedTree.item active computedTree
                    , [ geo
                      , panOffset
                      , dragOffset
                      ]
                        |> List.foldl Utils.addFloatTuples ( 0, 0 )
                    )
            )


{-| Sets a new item at the active node. This may be adding a completely new item from scratch (in case the current node is a placeholder), or modifying an existing one. Typically, the modification is based off an original value provided by the [activeNode](#activeNode) method.
-}
setActiveNode : item -> Model item -> Model item
setActiveNode newItem (Model model) =
    let
        tree =
            ComputedTree.tree model.computedTree

        flat =
            ComputedTree.flat model.computedTree

        newTree =
            model.active
                |> Maybe.map
                    (\active ->
                        let
                            item =
                                flat
                                    |> List.filter (\( path, _ ) -> active == path)
                                    |> List.head
                                    |> Maybe.map Tuple.second
                        in
                            case item of
                                Just (Just item) ->
                                    Tree.update active newItem tree

                                Just Nothing ->
                                    Tree.insert (List.take (List.length active - 1) active) (Just newItem) tree

                                _ ->
                                    -- Impossible state
                                    tree
                    )
                |> Maybe.withDefault tree
    in
        Model
            { model
                | computedTree = ComputedTree.init newTree
                , prevComputedTree = model.computedTree
            }


{-| Delete the active node from a tree, including all of its children. If a placeholder is active, this method does nothing.
-}
deleteActiveNode : Model item -> Model item
deleteActiveNode (Model model) =
    Model
        { model
            | computedTree =
                model.active
                    |> Maybe.map (\active -> Tree.delete active (ComputedTree.tree model.computedTree) |> ComputedTree.init)
                    |> Maybe.withDefault model.computedTree
            , prevComputedTree = model.computedTree
        }


{-| Subscriptions responsible for obtaining animation frames used when animating a view centering. See example for usage.
-}
subscriptions : Model item -> Sub (Msg item)
subscriptions (Model model) =
    if model.isReceivingAnimationFrames && model.targetPanOffset == Nothing then
        Sub.none
    else
        AnimationFrame.times Messages.AnimationFrameTick


{-| Access the current [tree](/Arborist-Tree).
-}
tree : Model item -> Arborist.Tree.Tree item
tree (Model { computedTree }) =
    ComputedTree.tree computedTree


{-| Msg type annotation for the program.
-}
type alias Msg item =
    Messages.Msg item


{-| A special update configuration object used in the custom [updateWith](/Arborist#updateWith) method. It holds the following fields:

    - `centerAt`: given the width and height of the canvas as float values, where should an activated node be centered. Defaults to `\width height -> ( width / 2, height / 2 )`

-}
type alias UpdateConfig =
    { centerAt : Float -> Float -> ( Float, Float )
    }


{-| A custom version of [update](/Arborist#update), using a [configuration](/Arborist#UpdateConfig).
-}
updateWith : UpdateConfig -> Config.Config item -> Msg item -> Model item -> Model item
updateWith { centerAt } config msg (Model model) =
    case msg of
        Messages.AnimationFrameTick time ->
            Model
                { model
                    | isReceivingAnimationFrames = True
                    , panOffset =
                        model.targetPanOffset
                            |> Maybe.map
                                (\( targetX, targetY ) ->
                                    let
                                        ( x, y ) =
                                            model.panOffset

                                        d =
                                            ((targetX - x) ^ 2 + (targetY - y) ^ 2) ^ 0.5

                                        dx =
                                            if (targetX == x) then
                                                0
                                            else
                                                (d / 10) * (targetX - x) / d

                                        dy =
                                            if (targetY == y) then
                                                0
                                            else
                                                (d / 10) * (targetY - y) / d
                                    in
                                        ( x + dx, y + dy )
                                )
                            |> Maybe.withDefault model.panOffset
                    , targetPanOffset =
                        model.targetPanOffset
                            |> Maybe.andThen
                                (\( targetX, targetY ) ->
                                    let
                                        ( x, y ) =
                                            model.panOffset
                                    in
                                        if abs (targetX - x) + abs (targetY - y) < 5 then
                                            Nothing
                                        else
                                            Just ( targetX, targetY )
                                )
                }

        Messages.NodeMouseDown isPlaceholder path x y ->
            Model
                { model
                    | drag =
                        if isPlaceholder then
                            MultiDrag.init
                        else
                            MultiDrag.start (Just path) x y
                    , active =
                        if isPlaceholder then
                            Just path
                        else
                            Nothing
                }

        Messages.NodeMouseUp x y ->
            let
                isPlaceholder =
                    (activeNode config (Model model) |> Maybe.map Tuple.first)
                        == (Just Nothing)

                active_ =
                    if isPlaceholder then
                        model.active
                    else
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

                layout =
                    ComputedTree.layout model.computedTree

                tree =
                    ComputedTree.tree model.computedTree

                newPanOffset =
                    active_
                        |> Maybe.andThen
                            (\active_ ->
                                nodeGeometry config active_ layout
                                    |> Maybe.map .center
                                    |> Maybe.map
                                        (\( cx, cy ) ->
                                            Utils.addFloatTuples
                                                (centerAt
                                                    config.layout.canvasWidth
                                                    config.layout.canvasHeight
                                                )
                                                ( -cx, -config.layout.nodeHeight / 2 - cy )
                                        )
                            )

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

                        -- If the client wired up the subscriptions, set the target pan offset to trigger the animation.
                        , targetPanOffset =
                            if model.isReceivingAnimationFrames then
                                newPanOffset
                            else
                                Nothing

                        -- Otherwise, center the view directly
                        , panOffset =
                            if model.isReceivingAnimationFrames then
                                model.panOffset
                            else
                                newPanOffset |> Maybe.withDefault model.panOffset
                        , active = active_
                        , isDragging = False
                    }

        Messages.NodeMouseEnter path ->
            Model { model | hovered = Just path }

        Messages.NodeMouseLeave path ->
            Model
                { model
                    | hovered =
                        if model.hovered == Just path then
                            Nothing
                        else
                            model.hovered
                }

        Messages.CanvasMouseMove xm ym ->
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

        Messages.CanvasMouseDown x y ->
            Model
                { model
                    | drag = MultiDrag.start Nothing x y
                }

        Messages.CanvasMouseUp x y ->
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
                    , active =
                        MultiDrag.state model.drag
                            |> Maybe.map
                                (\( path, ( x, y ) ) ->
                                    if (abs x + abs y) < 20 then
                                        Nothing
                                    else
                                        model.active
                                )
                            |> Maybe.withDefault Nothing
                }

        Messages.CanvasMouseLeave ->
            Model { model | drag = MultiDrag.init }

        Messages.NoOp ->
            Model model


{-| Update method, with the global editor [Config](/Arborist-Config#Config) as its first argument.
-}
update : Config.Config item -> Msg item -> Model item -> Model item
update =
    updateWith { centerAt = (\width height -> ( width / 2, height / 2 )) }


getDropTarget : Config.Config item -> TreeNodePath -> ( Float, Float ) -> ComputedTree.ComputedTree item -> Maybe TreeNodePath
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
            |> List.filterMap
                (\( path_, _ ) ->
                    let
                        ( xo, yo ) =
                            nodeGeometry config path_ layout
                                |> Maybe.map .center
                                |> Maybe.withDefault ( 0, 0 )

                        dx =
                            abs (x - xo)

                        dy =
                            abs (y - yo)
                    in
                        if
                            (List.all identity
                                [ not (Utils.startsWith path path_)
                                , not (Utils.startsWith path_ path)
                                , dx < config.layout.nodeWidth
                                , dy < config.layout.nodeHeight
                                ]
                            )
                        then
                            Just ( path_, dx + dy )
                        else
                            Nothing
                )
            |> List.sortWith
                (\( _, d1 ) ( _, d2 ) ->
                    if d1 > d2 then
                        GT
                    else if d1 == d2 then
                        EQ
                    else
                        LT
                )
            |> List.head
            |> Maybe.map Tuple.first


nodeGeometry : Config.Config item -> List Int -> Tree.Layout -> Maybe NodeGeometry
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
                        ( config.layout.canvasWidth / 2 + centerX * (config.layout.nodeWidth + config.layout.gutter)
                        , config.layout.canvasHeight * 0.1 + centerY * (config.layout.nodeHeight + config.layout.level)
                        )
                    , childCenters =
                        List.map
                            (\center ->
                                ( config.layout.canvasWidth / 2 + center * (config.layout.nodeWidth + config.layout.gutter)
                                , config.layout.canvasHeight * 0.1 + (centerY + 1) * (config.layout.nodeHeight + config.layout.level)
                                )
                            )
                            childCenters
                    }
            )


{-| The view, as a function of the `Config`, some custom html attributes for the container, and the Model.
-}
view : Config.Config item -> List (Attribute (Msg item)) -> Model item -> Html (Msg item)
view config attrs (Model model) =
    let
        flatTree =
            ComputedTree.flat model.computedTree

        layout =
            ComputedTree.layout model.computedTree

        nodeBaseStyle =
            Styles.nodeBase config.layout

        coordStyle =
            Styles.coordinate config.layout

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
                (Decode.map2 Messages.CanvasMouseMove
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                )
             , on "mousedown"
                (Decode.map2 Messages.CanvasMouseDown
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                )
             , on "mouseup"
                (Decode.map2 Messages.CanvasMouseUp
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                )
             , on "mouseleave" (Decode.succeed Messages.CanvasMouseLeave)
             , style
                [ ( "overflow", "hidden" )
                , ( "width", Utils.floatToPxString config.layout.canvasWidth )
                , ( "height", Utils.floatToPxString config.layout.canvasHeight )
                , ( "box-sizing", "border-box" )
                , ( "position", "relative" )
                , ( "cursor"
                  , if isCanvasDragging then
                        "move"
                    else
                        "auto"
                  )
                ]
             ]
                ++ attrs
            )
            [ div
                [ style
                    [ ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "position", "relative" )
                    , ( "transform", "translate3d(" ++ (Utils.floatToPxString canvasTotalDragOffsetX) ++ ", " ++ (Utils.floatToPxString canvasTotalDragOffsetY) ++ ", 0)" )
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

                                        modelIsDragging =
                                            model.isDragging

                                        ( isDragged, ( xDrag, yDrag ), draggedPath, isDropTarget ) =
                                            if modelIsDragging then
                                                dragState
                                                    |> Maybe.map
                                                        (\( draggedPath, offset ) ->
                                                            case draggedPath of
                                                                Just draggedPath ->
                                                                    let
                                                                        isDropTarget =
                                                                            getDropTarget config draggedPath offset model.computedTree
                                                                                |> Maybe.map (\dropTargetPath -> dropTargetPath == path)
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

                                        xWithDrag =
                                            x + xDrag

                                        yWithDrag =
                                            y + yDrag

                                        itemViewContext =
                                            { parent =
                                                flatTree
                                                    |> List.filterMap
                                                        (\( path_, item ) ->
                                                            if path_ == List.take (List.length path - 1) path then
                                                                item
                                                            else
                                                                Nothing
                                                        )
                                                    |> List.head
                                            , siblings =
                                                flatTree
                                                    |> List.filterMap
                                                        (\( path_, item ) ->
                                                            item
                                                                |> Maybe.andThen
                                                                    (\item ->
                                                                        if path /= path_ && List.length path == List.length path_ && List.take (List.length path_ - 1) path_ == List.take (List.length path - 1) path then
                                                                            Just item
                                                                        else
                                                                            Nothing
                                                                    )
                                                        )
                                            , children =
                                                flatTree
                                                    |> List.filterMap
                                                        (\( path_, item ) ->
                                                            item
                                                                |> Maybe.andThen
                                                                    (\item ->
                                                                        if List.length path_ == List.length path + 1 && List.take (List.length path) path_ == path then
                                                                            Just item
                                                                        else
                                                                            Nothing
                                                                    )
                                                        )
                                            , state =
                                                if (model.active == Just path) then
                                                    Config.Active
                                                else if isDropTarget then
                                                    Config.DropTarget
                                                else if (model.hovered == Just path) then
                                                    Config.Hovered
                                                else
                                                    Config.Normal
                                            }
                                    in
                                        [ div
                                            [ style <|
                                                nodeBaseStyle
                                                    ++ (coordStyle ( xWithDrag, yWithDrag ))
                                                    ++ (if isDragged then
                                                            [ ( "z-index", "100" )
                                                            , ( "cursor", "move" )
                                                            ]
                                                        else
                                                            []
                                                       )
                                            , Utils.onClickStopPropagation Messages.NoOp
                                            , onWithOptions "mousedown"
                                                { stopPropagation = True
                                                , preventDefault = False
                                                }
                                                (Decode.map2 (Messages.NodeMouseDown (item == Nothing) path)
                                                    (Decode.field "screenX" Decode.float)
                                                    (Decode.field "screenY" Decode.float)
                                                )
                                            , onWithOptions "mouseup"
                                                { stopPropagation = True
                                                , preventDefault = False
                                                }
                                                (Decode.map2 Messages.NodeMouseUp
                                                    (Decode.field "screenX" Decode.float)
                                                    (Decode.field "screenY" Decode.float)
                                                )
                                            , on "mouseenter" (Decode.succeed (Messages.NodeMouseEnter path))
                                            , on "mouseleave" (Decode.succeed (Messages.NodeMouseLeave path))
                                            ]
                                            [ config.view itemViewContext item
                                            ]
                                        ]
                                            ++ (if isDragged then
                                                    [ div
                                                        [ style <|
                                                            nodeBaseStyle
                                                                ++ Styles.dragShadowNode
                                                                ++ (coordStyle ( x, y ))
                                                        ]
                                                        []
                                                    ]
                                                        ++ (if item == Nothing then
                                                                []
                                                            else
                                                                [ Views.NodeConnectors.view config.layout 0.3 ( 0, 0 ) center childCenters ]
                                                           )
                                                else
                                                    []
                                               )
                                            ++ (if item == Nothing then
                                                    []
                                                else
                                                    [ Views.NodeConnectors.view config.layout 1.0 ( xDrag, yDrag ) center childCenters
                                                    ]
                                               )
                                )
                            |> Maybe.withDefault []
                    )
                    flatTree
                    |> List.foldl (++) []
                )
            ]
