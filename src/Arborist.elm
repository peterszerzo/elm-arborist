module Arborist exposing
    ( State, init, NodeView, view, subscriptions, Updater
    , Setting
    , hasActiveNode, activeNode, setActiveNode, setActiveNodeWithChildren, deleteActiveNode, updateActiveBranch
    , reposition, deactivate
    , NodeState(..), Context
    )

{-| Drag-and-drop interface to edit, dissect and-rearrange tree structures with arbitrary data sitting in their nodes.


# Module setup

@docs State, init, NodeView, view, subscriptions, Updater


# Configuration

@docs Setting


# Arborist tree getters and modifiers

@docs hasActiveNode, activeNode, setActiveNode, setActiveNodeWithChildren, deleteActiveNode, updateActiveBranch


# Display modifiers

@docs reposition, deactivate


# Context

@docs NodeState, Context

-}

import Arborist.Tree as Tree
import Browser.Events
import Dict
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Html.Keyed exposing (node)
import Internals.Drag as Drag exposing (Drag)
import Internals.NodeConnectors as NodeConnectors
import Internals.Offset as Offset
import Internals.Pt as Pt
import Internals.Settings as Settings
import Internals.Styles as Styles
import Internals.TreeUtils as TreeUtils exposing (Path)
import Internals.Utils as Utils
import Json.Decode as Decode


type alias NodeGeometry =
    { center : ( Float, Float )
    , childCenters : List ( Float, Float )
    }


{-| Opaque type for the editor's model, dependent on a node type variable. You can only use this for type annotation - to initialize a new model, see [init](#init).
-}
type State
    = State
        { active : Maybe Path
        , hovered : Maybe Path
        , drag : Drag (Maybe Path)
        , panOffset : Offset.Offset
        }


{-| Initialize state.
-}
init : State
init =
    State
        { active = Nothing
        , hovered = Nothing
        , drag = Drag.init
        , panOffset = Offset.noOffset
        }


{-| Type definition for the settings object
-}
type alias Setting node =
    Settings.Setting node


{-| Restores the original pan position of the tree.
-}
reposition : State -> State
reposition (State model) =
    State
        { model
            | panOffset = Offset.noOffset
            , drag = Drag.init
        }


{-| Remove active node
-}
deactivate : State -> State
deactivate (State model) =
    State
        { model
            | active = Nothing
        }


type alias ComputedTree node =
    { flat : List ( List Int, Maybe node )
    , layoutLazy : () -> TreeUtils.Layout
    }


computeTree :
    Settings.Settings node
    -> Tree.Tree node
    -> ComputedTree node
computeTree settings tree =
    let
        treeModifiedForCompute =
            tree
                |> TreeUtils.clusterBy settings.isNodeClustered
                |> (\newTree ->
                        case ( settings.dragAndDrop, settings.showPlaceholderLeaves, settings.showPlaceholderLeavesAdvanced ) of
                            ( True, _, _ ) ->
                                newTree

                            ( _, _, Just addEmpties ) ->
                                TreeUtils.addTrailingEmptiesAdvanced (\context -> addEmpties context && not (settings.isNodeClustered context.node)) newTree

                            ( _, True, _ ) ->
                                TreeUtils.addTrailingEmptiesAdvanced (\context -> not (settings.isNodeClustered context.node)) newTree

                            _ ->
                                newTree
                   )

        flat =
            TreeUtils.flatten treeModifiedForCompute
    in
    { flat = flat
    , layoutLazy =
        \_ ->
            treeModifiedForCompute
                |> TreeUtils.layout
    }


computeTreeFlatOnly :
    Settings.Settings node
    -> Tree.Tree node
    -> List ( List Int, Maybe node )
computeTreeFlatOnly settings tree =
    let
        treeModifiedForCompute =
            tree
                |> TreeUtils.clusterBy settings.isNodeClustered
                |> (\newTree ->
                        case ( settings.dragAndDrop, settings.showPlaceholderLeaves, settings.showPlaceholderLeavesAdvanced ) of
                            ( True, _, _ ) ->
                                newTree

                            ( _, _, Just addEmpties ) ->
                                TreeUtils.addTrailingEmptiesAdvanced (\context -> addEmpties context && not (settings.isNodeClustered context.node)) newTree

                            ( _, True, _ ) ->
                                TreeUtils.addTrailingEmptiesAdvanced (\context -> not (settings.isNodeClustered context.node)) newTree

                            _ ->
                                newTree
                   )
    in
    TreeUtils.flatten treeModifiedForCompute


{-| Returns the current active node as a tuple of `Maybe node` (as the node maybe a placeholder for a new node), as well as some contextual information as a two-field record:

  - `position : ( Float, Float )`: the node's position on the canvas (useful for rendering an edit pop-up).
  - `context`: view context, identical to the one provided in [NodeView](#NodeView).

In order for the position calculations to match the current active node, you must supply the same settings array that the [view](#view) method gets.

-}
activeNode :
    { settings : List (Setting node)
    , state : State
    , tree : Tree.Tree node
    }
    ->
        Maybe
            ( Maybe node
            , { position : ( Float, Float )
              , context : Context node
              }
            )
activeNode config =
    let
        (State model) =
            config.state

        { tree } =
            config

        settings =
            Settings.apply config.settings Settings.defaults
    in
    model.active
        |> Maybe.map
            (\active ->
                let
                    computedTree =
                        computeTree settings tree

                    layout =
                        computedTree.layoutLazy ()

                    geo =
                        nodeGeometry settings active layout
                            |> Maybe.map .center
                            |> Maybe.withDefault ( 0, 0 )

                    dragOffset =
                        Drag.state model.drag
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault Offset.noOffset

                    offsetConfig =
                        Utils.offsetConfig settings
                in
                ( computedTree.flat
                    |> List.filter (\( path_, _ ) -> active == path_)
                    |> List.head
                    |> Maybe.andThen Tuple.second
                , { position =
                        [ geo
                        , model.panOffset
                            |> Offset.toPt offsetConfig
                        , dragOffset
                            |> Offset.toPt offsetConfig
                        ]
                            |> List.foldl Pt.add ( 0, 0 )
                  , context = viewContext settings computedTree (State model) tree active
                  }
                )
            )


{-| Determines whether there is an active node. This is a simple alternative to [activeNode](#activeNode), which provides a lot more information and requires more inputs in scope.
-}
hasActiveNode : State -> Bool
hasActiveNode (State state) =
    state.active /= Nothing


{-| Sets a new node at the active position. This may be adding a completely new node from scratch (in case the current node is a placeholder), or modifying an existing one. Typically, the modification is based off an original value provided by the [activeNode](#activeNode) method.
-}
setActiveNode : node -> State -> Tree.Tree node -> ( State, Tree.Tree node )
setActiveNode newNode =
    setActiveNodeWithChildren
        { node = newNode
        , childrenOverride = Nothing
        }


{-| Similar to [setActiveNode](#setActiveNode), but instead of updating just the node with its subtrees intact, it updates the entire branch based on a `Tree -> Tree` function.
-}
updateActiveBranch : (Tree.Tree node -> Tree.Tree node) -> State -> Tree.Tree node -> ( State, Tree.Tree node )
updateActiveBranch fn (State state) tree =
    ( State state
    , state.active
        |> Maybe.map
            (\active ->
                TreeUtils.updateAt
                    active
                    fn
                    tree
            )
        |> Maybe.withDefault tree
    )


{-| Sets the active node with the option to also set its children. The existing children will be discarded along with their children.
-}
setActiveNodeWithChildren :
    { node : node
    , childrenOverride : Maybe (List node)
    }
    -> State
    -> Tree.Tree node
    -> ( State, Tree.Tree node )
setActiveNodeWithChildren newStuff (State model) tree =
    ( State model
    , model.active
        |> Maybe.map
            (\active ->
                TreeUtils.updateAt
                    active
                    (\branch ->
                        case branch of
                            Tree.Empty ->
                                Tree.Node newStuff.node
                                    (newStuff.childrenOverride
                                        |> Maybe.withDefault []
                                        |> List.map (\child -> Tree.Node child [])
                                    )

                            Tree.Node _ existingChildren ->
                                Tree.Node newStuff.node
                                    (newStuff.childrenOverride
                                        |> Maybe.map (List.map (\child -> Tree.Node child []))
                                        |> Maybe.withDefault existingChildren
                                    )
                    )
                    tree
            )
        |> Maybe.withDefault tree
    )


{-| Delete the active node from a tree, including all of its children. If a placeholder is active, this method does nothing.
-}
deleteActiveNode : State -> Tree.Tree node -> ( State, Tree.Tree node )
deleteActiveNode (State state) tree =
    ( State
        { state
            | active = Nothing
        }
    , state.active
        |> Maybe.map
            (\active ->
                TreeUtils.delete active
                    tree
            )
        |> Maybe.withDefault tree
    )


{-| Module messages
-}
type Msg
    = NodeMouseDown Path Float Float
    | NodeMouseUp (Maybe Path) Float Float
    | NodeMouseEnter Path
    | NodeMouseLeave Path
    | CanvasMouseMove Float Float
    | CanvasMouseDown Float Float
    | CanvasMouseUp Float Float
    | CanvasMouseLeave


resolveDrop : Settings.Settings node -> State -> Tree.Tree node -> Tree.Tree node
resolveDrop settings (State model) tree =
    let
        flat =
            computeTreeFlatOnly settings tree

        stuff =
            Drag.state model.drag
                |> Maybe.andThen
                    (\( path, dragOffset ) ->
                        Maybe.map
                            (\justPath ->
                                ( justPath
                                , dragOffset
                                )
                            )
                            path
                    )
                |> Maybe.andThen
                    (\( path, dragOffset ) ->
                        getDropTarget settings path (Offset.toPt (Utils.offsetConfig settings) dragOffset) tree
                            |> Maybe.map
                                (\dropTargetPath ->
                                    ( path
                                    , dropTargetPath
                                    )
                                )
                    )
    in
    stuff
        |> Maybe.map
            (\( path, dropTargetPath ) ->
                flat
                    |> List.filter (\( currentPath, _ ) -> currentPath == dropTargetPath)
                    |> List.head
                    |> Maybe.andThen
                        (\( currentPath, currentNode ) ->
                            Maybe.map
                                (\realNode ->
                                    ( currentPath, realNode )
                                )
                                currentNode
                        )
                    |> Maybe.map (\_ -> TreeUtils.swap path dropTargetPath tree)
                    |> Maybe.withDefault
                        -- If the drop target is a placeholder, first add an Empty node in the original tree
                        -- so the swap method actually finds a node.
                        (TreeUtils.insert
                            (List.take
                                (List.length dropTargetPath - 1)
                                dropTargetPath
                            )
                            Tree.Empty
                            tree
                            |> TreeUtils.swap path dropTargetPath
                            |> TreeUtils.removeEmpties
                        )
            )
        |> Maybe.withDefault tree


resolvePanOffset : Settings.Settings node -> Drag.Drag (Maybe Path) -> Offset.Offset -> Offset.Offset
resolvePanOffset settings drag panOffset =
    Drag.state drag
        |> Maybe.map
            (\( draggedNode, dragOffset ) ->
                let
                    ( panOffsetX, panOffsetY ) =
                        panOffset
                            |> Offset.toPt (Utils.offsetConfig settings)

                    ( dragOffsetX, dragOffsetY ) =
                        if draggedNode == Nothing then
                            Offset.toPt (Utils.offsetConfig settings) dragOffset

                        else
                            ( 0, 0 )
                in
                ( panOffsetX + dragOffsetX, panOffsetY + dragOffsetY )
                    |> Offset.fromPt (Utils.offsetConfig settings)
            )
        |> Maybe.withDefault panOffset


activePanOffset : Path -> Settings.Settings node -> Tree.Tree node -> Maybe Offset.Offset
activePanOffset active settings tree =
    let
        { layoutLazy } =
            computeTree settings tree

        layout =
            layoutLazy ()
    in
    nodeGeometry settings active layout
        |> Maybe.map
            (.center
                >> (\( cx, cy ) ->
                        [ settings.centerOffset
                        , ( settings.canvasWidth / 2
                          , settings.canvasHeight / 2
                          )
                        , ( -cx, -settings.nodeHeight / 2 - cy )
                        ]
                            |> List.foldl Pt.add ( 0, 0 )
                   )
                >> Offset.fromPt (Utils.offsetConfig settings)
            )


isDragOffsetSubstantial : Settings.Settings node -> Offset.Offset -> Bool
isDragOffsetSubstantial settings offset =
    let
        ( offsetX, offsetY ) =
            Offset.toPt (Utils.offsetConfig settings) offset
    in
    abs offsetX + abs offsetY > 20


{-| Update function handling changes in the state.
-}
update : Settings.Settings node -> Msg -> State -> Tree.Tree node -> ( State, Tree.Tree node )
update settings msg (State state) tree =
    case msg of
        NodeMouseDown path x y ->
            let
                flat =
                    computeTreeFlatOnly settings tree
            in
            if settings.dragAndDrop then
                ( State
                    { state
                        | drag = Drag.start (Just path) x y
                        , active = Just path
                    }
                , tree
                )

            else
                let
                    newTree =
                        case Utils.dictGetWithListKeys path (Dict.fromList flat) of
                            Just (Just _) ->
                                tree

                            -- A placeholder has been clicked. Add new node to the tree
                            _ ->
                                case settings.defaultNode of
                                    Just defaultNode ->
                                        TreeUtils.insert
                                            path
                                            (Tree.Node defaultNode
                                                []
                                            )
                                            tree

                                    Nothing ->
                                        tree
                in
                ( State
                    { state
                        | drag = Drag.init
                        , active = Just path
                        , panOffset =
                            activePanOffset path settings newTree
                                |> Maybe.withDefault state.panOffset
                    }
                , newTree
                )

        NodeMouseUp _ _ _ ->
            let
                newActive =
                    if not settings.dragAndDrop then
                        state.active

                    else
                        Drag.state state.drag
                            |> Maybe.andThen
                                (\( path, dragOffset ) ->
                                    case path of
                                        Just _ ->
                                            -- If the node was dragged far enough already, it is not activated
                                            -- This case also protects from reading a slightly sloppy click as a drag
                                            if isDragOffsetSubstantial settings dragOffset then
                                                Nothing

                                            else
                                                path

                                        Nothing ->
                                            Nothing
                                )

                newTree =
                    resolveDrop settings (State state) tree

                newPanOffset =
                    newActive
                        |> Maybe.andThen
                            (\currentActive ->
                                activePanOffset currentActive settings newTree
                            )
                        |> Maybe.withDefault state.panOffset
            in
            ( State
                { state
                    | drag = Drag.init
                    , panOffset = newPanOffset
                    , active = newActive
                }
            , newTree
            )

        NodeMouseEnter path ->
            ( State
                { state
                    | hovered =
                        Just path
                }
            , tree
            )

        NodeMouseLeave _ ->
            ( State
                { state
                    | hovered = Nothing
                }
            , tree
            )

        CanvasMouseMove xm ym ->
            ( State
                { state
                    | drag = Drag.move (Utils.offsetConfig settings) xm ym state.drag
                }
            , tree
            )

        CanvasMouseDown x y ->
            case state.hovered of
                Just path ->
                    update settings (NodeMouseDown path x y) (State state) tree

                Nothing ->
                    ( State
                        { state
                            | drag = Drag.start Nothing x y
                        }
                    , tree
                    )

        CanvasMouseUp x y ->
            case ( state.hovered, settings.dragAndDrop ) of
                ( Just _, True ) ->
                    update settings (NodeMouseUp Nothing x y) (State state) tree

                _ ->
                    ( State
                        { state
                            | drag = Drag.init
                            , panOffset = resolvePanOffset settings state.drag state.panOffset
                            , active =
                                case Drag.state state.drag of
                                    Just ( _, dragOffset ) ->
                                        if isDragOffsetSubstantial settings dragOffset then
                                            state.active

                                        else
                                            Nothing

                                    Nothing ->
                                        state.active
                        }
                    , tree
                    )

        CanvasMouseLeave ->
            ( State
                { state
                    | drag = Drag.init
                    , panOffset = resolvePanOffset settings state.drag state.panOffset
                }
            , tree
            )


type ArrowDirection
    = ArrowDown
    | ArrowUp
    | ArrowLeft
    | ArrowRight


isOutsideDecoder : String -> Decode.Decoder Bool
isOutsideDecoder containerId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if containerId == id then
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "continue"
                )
        , Decode.lazy (\_ -> isOutsideDecoder containerId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]


keyDecoder : Decode.Decoder ArrowDirection
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowDown" ->
                        Decode.succeed ArrowDown

                    "ArrowUp" ->
                        Decode.succeed ArrowUp

                    "ArrowLeft" ->
                        Decode.succeed ArrowLeft

                    "ArrowRight" ->
                        Decode.succeed ArrowRight

                    _ ->
                        Decode.fail "No arrow key detected. This is fine"
            )


{-| Subscriptions for interactive enhancements like keyboard events
-}
subscriptions : List (Setting node) -> State -> Tree.Tree node -> Sub ( State, Tree.Tree node )
subscriptions settingsOverrides (State state) tree =
    let
        settings =
            Settings.apply settingsOverrides Settings.defaults

        { flat, layoutLazy } =
            computeTree settings tree
    in
    case settings.keyboardNavigation of
        Nothing ->
            Sub.none

        Just keyboardNavigationConfig ->
            Browser.Events.onKeyDown
                ((case keyboardNavigationConfig.outsideId of
                    Just domId ->
                        Decode.map2
                            (\key isOutside ->
                                if isOutside then
                                    Just key

                                else
                                    Nothing
                            )
                            keyDecoder
                            (Decode.field "target" (isOutsideDecoder domId))
                            |> Decode.andThen
                                (\maybeKey ->
                                    case maybeKey of
                                        Just key ->
                                            Decode.succeed key

                                        Nothing ->
                                            Decode.fail "Not outside specified ID"
                                )

                    Nothing ->
                        keyDecoder
                 )
                    |> Decode.andThen
                        (\arrowDirection ->
                            let
                                all =
                                    List.map Tuple.first flat

                                newActive =
                                    case arrowDirection of
                                        ArrowDown ->
                                            TreeUtils.moveDown all state.active

                                        ArrowUp ->
                                            TreeUtils.moveUp all state.active

                                        ArrowLeft ->
                                            TreeUtils.moveLeft all state.active

                                        ArrowRight ->
                                            TreeUtils.moveRight all state.active

                                newPanOffset =
                                    newActive
                                        |> Maybe.andThen
                                            (\justActive ->
                                                let
                                                    layout =
                                                        layoutLazy ()
                                                in
                                                nodeGeometry settings justActive layout
                                                    |> Maybe.map
                                                        (.center
                                                            >> (\( cx, cy ) ->
                                                                    [ settings.centerOffset
                                                                    , ( settings.canvasWidth / 2
                                                                      , settings.canvasHeight / 2
                                                                      )
                                                                    , ( -cx, -settings.nodeHeight / 2 - cy )
                                                                    ]
                                                                        |> List.foldl Pt.add ( 0, 0 )
                                                               )
                                                            >> Offset.fromPt (Utils.offsetConfig settings)
                                                        )
                                            )
                                        |> Maybe.withDefault state.panOffset
                            in
                            Decode.succeed
                                ( State
                                    { state
                                        | active = newActive
                                        , panOffset = newPanOffset
                                    }
                                , tree
                                )
                        )
                )


getDropTarget :
    Settings.Settings node
    -> Path
    -> ( Float, Float )
    -> Tree.Tree node
    -> Maybe Path
getDropTarget settings path ( dragX, dragY ) tree =
    let
        computedTree =
            computeTree settings tree

        layout =
            computedTree.layoutLazy ()

        ( x0, y0 ) =
            nodeGeometry settings path layout
                |> Maybe.map .center
                |> Maybe.withDefault ( 0, 0 )

        ( x, y ) =
            ( x0 + dragX
            , y0 + dragY
            )
    in
    computedTree.flat
        |> List.filterMap
            (\( path_, node ) ->
                let
                    ( xo, yo ) =
                        nodeGeometry settings path_ layout
                            |> Maybe.map .center
                            |> Maybe.withDefault ( 0, 0 )

                    dx =
                        abs (x - xo)

                    dy =
                        abs (y - yo)
                in
                if
                    List.all identity
                        [ not (Utils.startsWith path path_)
                        , not (Utils.startsWith path_ path)
                        , dx < settings.nodeWidth
                        , dy < settings.nodeHeight
                        ]
                then
                    Just ( path_, dx + dy, node )

                else
                    Nothing
            )
        |> List.sortWith
            (\( _, d1, _ ) ( _, d2, _ ) ->
                if d1 > d2 then
                    GT

                else if d1 == d2 then
                    EQ

                else
                    LT
            )
        |> List.head
        |> Maybe.andThen
            (\( dropTargetPath, _, dropTargetNode ) ->
                if
                    -- Can't drop node on an empty sibling
                    ((path |> List.reverse |> List.tail) == (dropTargetPath |> List.reverse |> List.tail))
                        && (dropTargetNode == Nothing)
                then
                    Nothing

                else
                    Just dropTargetPath
            )


nodeGeometry : Settings.Settings node -> List Int -> TreeUtils.Layout -> Maybe NodeGeometry
nodeGeometry settings path layout =
    layout
        |> Utils.dictGetWithListKeys path
        |> Maybe.map
            (\{ center, childCenters } ->
                let
                    ( centerX, centerY ) =
                        center
                in
                { center =
                    ( settings.canvasWidth * 0.5 + centerX * (settings.nodeWidth + settings.gutter)
                    , settings.canvasHeight * 0.5 + centerY * (settings.nodeHeight + settings.level)
                    )
                , childCenters =
                    List.map
                        (\childCenter ->
                            ( settings.canvasWidth * 0.5 + childCenter * (settings.nodeWidth + settings.gutter)
                            , settings.canvasHeight * 0.5 + (centerY + 1) * (settings.nodeHeight + settings.level)
                            )
                        )
                        childCenters
                }
            )


{-| View function for an individual node, depending on its [context](Context), and its value. This value is expressed as a maybe because the node may contain an `insert new node`-type placeholder.
-}
type alias NodeView node msg =
    Context node -> Maybe node -> Html.Html msg


isSibling : List Int -> List Int -> Bool
isSibling nodePath1 nodePath2 =
    nodePath1
        /= nodePath2
        && List.length nodePath1
        == List.length nodePath2
        && List.take (List.length nodePath2 - 1) nodePath2
        == List.take (List.length nodePath1 - 1) nodePath1


viewContext :
    Settings.Settings node
    -> ComputedTree node
    -> State
    -> Tree.Tree node
    -> List Int
    -> Context node
viewContext settings computedTree (State model) tree path =
    let
        { flat } =
            computedTree

        isDropTarget =
            Drag.state model.drag
                |> Maybe.map
                    (\( draggedPath, dragOffset ) ->
                        case draggedPath of
                            Just justDraggedPath ->
                                let
                                    offset =
                                        Offset.toPt (Utils.offsetConfig settings) dragOffset
                                in
                                getDropTarget settings justDraggedPath offset tree
                                    |> Maybe.map (\dropTargetPath -> dropTargetPath == path)
                                    |> Maybe.withDefault False

                            Nothing ->
                                False
                    )
                |> Maybe.withDefault False

        nodeViewContext =
            { parent =
                flat
                    |> List.filterMap
                        (\( path_, node ) ->
                            -- List.take -1 path == List.take 0 path, hence the additional path /= [] condition
                            if path /= [] && path_ == List.take (List.length path - 1) path then
                                node

                            else
                                Nothing
                        )
                    |> List.head
            , siblings =
                flat
                    |> List.filterMap
                        (\( path_, node ) ->
                            node
                                |> Maybe.andThen
                                    (\justNode ->
                                        if isSibling path path_ then
                                            Just justNode

                                        else
                                            Nothing
                                    )
                        )
            , children =
                flat
                    |> List.filterMap
                        (\( path_, node ) ->
                            node
                                |> Maybe.andThen
                                    (\justNode ->
                                        if
                                            List.length path_
                                                == List.length path
                                                + 1
                                                && List.take (List.length path) path_
                                                == path
                                        then
                                            Just justNode

                                        else
                                            Nothing
                                    )
                        )
            , state =
                if model.active == Just path then
                    Active

                else if isDropTarget then
                    DropTarget

                else if model.hovered == Just path then
                    Hovered

                else
                    Normal
            }
    in
    nodeViewContext


nodeDragInfo : Settings.Settings node -> List Int -> State -> ( Bool, ( Float, Float ) )
nodeDragInfo settings path (State model) =
    let
        modelIsDragging =
            Drag.state model.drag /= Nothing

        dragState =
            Drag.state model.drag
    in
    if modelIsDragging then
        dragState
            |> Maybe.map
                (\( draggedPath, dragOffset ) ->
                    case draggedPath of
                        Just justDraggedPath ->
                            let
                                offset =
                                    Offset.toPt (Utils.offsetConfig settings) dragOffset
                            in
                            if Utils.startsWith justDraggedPath path then
                                ( True, offset )

                            else
                                ( False, ( 0, 0 ) )

                        Nothing ->
                            ( False, ( 0, 0 ) )
                )
            |> Maybe.withDefault ( False, ( 0, 0 ) )

    else
        ( False, ( 0, 0 ) )


type alias Config node msg =
    { nodeView : NodeView node msg
    , tree : Tree.Tree node
    , state : State
    , toMsg : Updater node -> msg
    , settings : List (Setting node)
    }


mapMsg : Config node msg -> Msg -> msg
mapMsg config msg =
    let
        settings =
            Settings.apply config.settings Settings.defaults
    in
    config.toMsg
        (\prevState prevTree ->
            update settings msg prevState prevTree
        )


{-| A function that updates hidden state and tree. This function is passed in the `toMsg` field of the [view function](#view).

Passing functions is necessary here because in the update function using `elm-arborist`, modifications in state always need to operate on the latest value because events like mousemove are fired very frequently and therefore it is possible that the changes caused by one event in the runtime are undone by one that follows immediately after.

-}
type alias Updater node =
    State -> Tree.Tree node -> ( State, Tree.Tree node )


isBetween : Float -> Float -> Float -> Bool
isBetween min max val =
    val >= min && val <= max


{-| The editor's view function, taking the following arguments:

  - [NodeView](#NodeView): view function for an individual node.
  - a list of html attributes for the container element.
  - the editor's [model](#State).

-}
view :
    List (Html.Attribute msg)
    ->
        { nodeView : NodeView node msg
        , tree : Tree.Tree node
        , state : State
        , toMsg : Updater node -> msg
        , settings : List (Setting node)
        }
    -> Html.Html msg
view attrs config =
    let
        (State model) =
            config.state

        { nodeView } =
            config

        settings =
            Settings.apply config.settings Settings.defaults

        computedTree =
            computeTree settings config.tree

        { flat, layoutLazy } =
            computedTree

        layout =
            layoutLazy ()

        nodeBaseStyle =
            Styles.nodeBase settings

        coordStyle =
            Styles.coordinate settings

        dragState =
            Drag.state model.drag

        mapMsg_ =
            mapMsg config

        ( canvasDragOffset, isCanvasDragging ) =
            dragState
                |> Maybe.map
                    (\( path, dragOffset ) ->
                        if path == Nothing then
                            let
                                offset =
                                    Offset.toPt (Utils.offsetConfig settings) dragOffset
                            in
                            ( offset, True )

                        else
                            ( ( 0, 0 ), False )
                    )
                |> Maybe.withDefault ( ( 0, 0 ), False )

        ( canvasTotalDragOffsetX, canvasTotalDragOffsetY ) =
            Pt.add canvasDragOffset
                (model.panOffset
                    |> Offset.toPt (Utils.offsetConfig settings)
                )
    in
    div
        ((if Drag.state model.drag /= Nothing then
            [ on "mousemove"
                (Decode.map2 CanvasMouseMove
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                    |> Decode.map mapMsg_
                )
            ]

          else
            []
         )
            ++ [ on "mousedown"
                    (Decode.map2 CanvasMouseDown
                        (Decode.field "screenX" Decode.float)
                        (Decode.field "screenY" Decode.float)
                        |> Decode.map mapMsg_
                    )
               , on "mouseup"
                    (Decode.map2 CanvasMouseUp
                        (Decode.field "screenX" Decode.float)
                        (Decode.field "screenY" Decode.float)
                        |> Decode.map mapMsg_
                    )
               , on "mouseleave"
                    (Decode.succeed CanvasMouseLeave
                        |> Decode.map mapMsg_
                    )
               , style "width" <| Utils.floatToPxString settings.canvasWidth
               , style "height" <| Utils.floatToPxString settings.canvasHeight
               , style "cursor" <|
                    if isCanvasDragging then
                        "move"

                    else
                        "auto"
               , style "overflow" "hidden"
               , style "box-sizing" "border-box"
               , style "position" "relative"
               ]
            ++ attrs
        )
        [ node "div"
            [ style "width" "100%"
            , style "height" "100%"
            , style "position" "relative"
            , style "transition" <|
                if Drag.state model.drag == Nothing then
                    "transform 0.3s ease-in-out"

                else
                    "none"
            , style
                "transform"
              <|
                "translate3d("
                    ++ Utils.floatToPxString canvasTotalDragOffsetX
                    ++ ", "
                    ++ Utils.floatToPxString canvasTotalDragOffsetY
                    ++ ", 0)"
            ]
          <|
            (List.map
                (\( path, node ) ->
                    let
                        pathIdBase =
                            path
                                |> List.map String.fromInt
                                |> String.join "-"
                    in
                    nodeGeometry settings path layout
                        |> Maybe.map
                            (\{ center, childCenters } ->
                                let
                                    ( x, y ) =
                                        center

                                    boundsTolerance =
                                        10

                                    isOffScreenX =
                                        not
                                            (x
                                                + canvasTotalDragOffsetX
                                                |> isBetween
                                                    (-settings.nodeWidth / 2 * boundsTolerance - settings.gutter)
                                                    (settings.canvasWidth + settings.nodeWidth / 2 * boundsTolerance + settings.gutter)
                                            )

                                    isOffScreenY =
                                        not
                                            (y
                                                + canvasTotalDragOffsetY
                                                |> isBetween
                                                    (-settings.nodeHeight / 2 * boundsTolerance - settings.level)
                                                    (settings.canvasHeight + settings.nodeHeight / 2 * boundsTolerance + settings.level)
                                            )

                                    isOffScreen =
                                        isOffScreenX || isOffScreenY

                                    ( isDragged, ( xDrag, yDrag ) ) =
                                        nodeDragInfo settings path (State model)

                                    xWithDrag =
                                        x + xDrag

                                    yWithDrag =
                                        y + yDrag

                                    nodeViewContext =
                                        viewContext
                                            settings
                                            computedTree
                                            (State model)
                                            config.tree
                                            path
                                in
                                (if isOffScreen then
                                    []

                                 else
                                    [ ( pathIdBase ++ "-base"
                                      , div
                                            ((nodeBaseStyle
                                                ++ coordStyle ( xWithDrag, yWithDrag )
                                                ++ (if isDragged then
                                                        [ ( "z-index", "100" )
                                                        , ( "cursor", "move" )
                                                        ]

                                                    else
                                                        []
                                                   )
                                                |> List.map
                                                    (\( property, value ) ->
                                                        style property
                                                            value
                                                    )
                                             )
                                                ++ [ on "mouseenter"
                                                        (Decode.succeed
                                                            (NodeMouseEnter path
                                                                |> mapMsg_
                                                            )
                                                        )
                                                   , on "mouseleave"
                                                        (Decode.succeed
                                                            (NodeMouseLeave path
                                                                |> mapMsg_
                                                            )
                                                        )
                                                   ]
                                            )
                                            [ nodeView nodeViewContext node
                                            ]
                                      )
                                    ]
                                )
                                    ++ (if isOffScreenY then
                                            []

                                        else
                                            let
                                                extendBy =
                                                    case ( settings.extendConnectorsBy, settings.extendConnectorsByAdvanced ) of
                                                        ( Just distance, _ ) ->
                                                            Just distance

                                                        ( _, Just fn ) ->
                                                            fn
                                                                { node = node
                                                                , parent = nodeViewContext.parent
                                                                , siblings = nodeViewContext.siblings
                                                                , children = nodeViewContext.children
                                                                }
                                                                |> Maybe.map toFloat

                                                        _ ->
                                                            Nothing
                                            in
                                            NodeConnectors.view
                                                { settings = settings
                                                , opacity = 1.0
                                                , offset = ( xDrag, yDrag )
                                                , center = center
                                                , childCenters = childCenters
                                                , extendBy = extendBy
                                                , extendTop = nodeViewContext.parent == Nothing
                                                , extendBottom = List.length nodeViewContext.children == 0
                                                }
                                                |> List.indexedMap
                                                    (\connectorIndex connectorElement ->
                                                        ( pathIdBase ++ "-" ++ String.fromInt connectorIndex ++ "-connector"
                                                        , if node == Nothing then
                                                            text ""

                                                          else
                                                            connectorElement
                                                        )
                                                    )
                                       )
                                    ++ (if isDragged && (abs xDrag + abs yDrag > 60) then
                                            ( pathIdBase ++ "-shadow"
                                            , div
                                                ((nodeBaseStyle
                                                    ++ coordStyle ( x, y )
                                                    |> List.map (\( property, value ) -> style property value)
                                                 )
                                                    ++ [ style "border-width" <| settings.connectorStrokeWidth ++ "px"
                                                       , style "border-style" "solid"
                                                       , style "box-sizing" "border-box"
                                                       , style "opacity" "0.3"
                                                       , style "border-color" <| settings.connectorStroke
                                                       ]
                                                )
                                                []
                                            )
                                                :: (if node == Nothing then
                                                        []

                                                    else
                                                        NodeConnectors.view
                                                            { settings = settings
                                                            , opacity = 0.3
                                                            , offset = ( 0, 0 )
                                                            , center = center
                                                            , childCenters = childCenters
                                                            , extendBy = Nothing
                                                            , extendTop = False
                                                            , extendBottom = False
                                                            }
                                                            |> List.indexedMap
                                                                (\connectorIndex connectorElement ->
                                                                    ( pathIdBase ++ "-" ++ String.fromInt connectorIndex ++ "-shadowconnector"
                                                                    , connectorElement
                                                                    )
                                                                )
                                                   )

                                        else
                                            []
                                       )
                            )
                        |> Maybe.withDefault []
                )
                flat
                |> List.foldl (++) []
            )
        ]


{-| The state of a node at a given time. May be normal one of the following:

  - `Normal`: node in rest state
  - `Hovered`: a hovered over node
  - `Active`: an activated node. Overrides hover
  - `DropTarget`: indicates that a swap or insert of the dragged subtree will take place at this node upon release

-}
type NodeState
    = Normal
    | Active
    | Hovered
    | DropTarget


{-| View context. Contains the following fields:

  - `parent`: the item at the parent, not available for the root node
  - `siblings`: a list of all direct siblings
  - `children`: a list of all direct children
  - `state`: node [state](#NodeState)

-}
type alias Context item =
    { parent : Maybe item
    , siblings : List item
    , children : List item
    , state : NodeState
    }
