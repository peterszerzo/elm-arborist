module Arborist exposing
    ( State, init, NodeView, view
    , Setting
    , activeNode, setActiveNode, setActiveNodeWithChildren, deleteActiveNode
    , reposition, deactivate
    , NodeState(..), Context
    )

{-| Drag-and-drop interface to edit, dissect and-rearrange tree structures with arbitrary data sitting in their nodes. Structured as a TEA component defining its own init, update and view, `elm-arborist` allows you to easily initialize a tree editor, keep its state in an opaque model, and access the [edited result at any time](#tree).


# Module setup

@docs State, init, NodeView, view


# Configuration

@docs Setting


# Arborist tree getters and modifiers

@docs activeNode, setActiveNode, setActiveNodeWithChildren, deleteActiveNode


# Display modifiers

@docs reposition, deactivate


# Context

@docs NodeState, Context

-}

import Arborist.Tree as Tree
import Css exposing (..)
import Dict
import Drag exposing (Drag)
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, stopPropagationOn)
import Internal.Settings as Settings
import Internal.Tree.Extra as TreeExtra exposing (TreeNodePath)
import Json.Decode as Decode
import Utils
import Views.NodeConnectors
import Views.Styles as Styles


type alias NodeGeometry =
    { center : ( Float, Float )
    , childCenters : List ( Float, Float )
    }


{-| Opaque type for the editor's model, dependent on a node type variable. You can only use this for type annotation - to initialize a new model, see [init](#init).
-}
type State node
    = State
        { active : Maybe TreeNodePath
        , hovered : Maybe TreeNodePath
        , drag : Drag (Maybe TreeNodePath)
        , panOffset : ( Float, Float )
        }


{-| Initialize state.
-}
init : State node
init =
    State
        { active = Nothing
        , hovered = Nothing
        , drag = Drag.init
        , panOffset = ( 0, 0 )
        }


{-| Type definition for the settings object
-}
type alias Setting node =
    Settings.Setting node


{-| Restores the original pan position of the tree.
-}
reposition : State node -> State node
reposition (State model) =
    State
        { model
            | panOffset = ( 0, 0 )
            , drag = Drag.init
        }


{-| Remove active node
-}
deactivate : State node -> State node
deactivate (State model) =
    State
        { model
            | active = Nothing
        }


computeTree : Settings.Settings node -> Tree.Tree node -> { flat : List ( List Int, Maybe node ), layout : TreeExtra.Layout }
computeTree settings tree =
    let
        withPlaceholders =
            case ( settings.showPlaceholderLeaves, settings.showPlaceholderLeavesAdvanced ) of
                ( _, Just addEmpties ) ->
                    TreeExtra.addTrailingEmptiesAdvanced addEmpties tree

                ( True, _ ) ->
                    TreeExtra.addTrailingEmpties tree

                ( _, _ ) ->
                    tree

        flat =
            TreeExtra.flatten withPlaceholders

        layout =
            withPlaceholders
                |> TreeExtra.layout
    in
    { layout = layout
    , flat = flat
    }


{-| Returns the current active node as a tuple of `Maybe node` (as the node maybe a placeholder for a new node), as well as some contextual information as a two-field record:

  - `position : ( Float, Float )`: the node's position on the canvas (useful for rendering an edit pop-up).
  - `context`: view context, identical to the one provided in [NodeView](#NodeView).

-}
activeNode : List (Setting node) -> State node -> Tree.Tree node -> Maybe ( Maybe node, { position : ( Float, Float ), context : Context node } )
activeNode settingsOverrides (State model) tree =
    let
        settings =
            Settings.apply settingsOverrides Settings.defaults
    in
    model.active
        |> Maybe.map
            (\active ->
                let
                    { layout, flat } =
                        computeTree settings tree

                    geo =
                        nodeGeometry settings active layout
                            |> Maybe.map .center
                            |> Maybe.withDefault ( 0, 0 )

                    dragOffset =
                        Drag.state model.drag
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault ( 0, 0 )
                in
                ( flat
                    |> List.filter (\( path_, _ ) -> active == path_)
                    |> List.head
                    |> Maybe.andThen Tuple.second
                , { position =
                        [ geo
                        , model.panOffset
                        , dragOffset
                        ]
                            |> List.foldl Utils.addFloatTuples ( 0, 0 )
                  , context = viewContext settings (State model) tree active
                  }
                )
            )


{-| Sets a new node at the active position. This may be adding a completely new node from scratch (in case the current node is a placeholder), or modifying an existing one. Typically, the modification is based off an original value provided by the [activeNodeWithContext](#activeNodeWithContext) method.
-}
setActiveNode : node -> State node -> Tree.Tree node -> Tree.Tree node
setActiveNode newNode (State model) tree =
    setActiveNodeWithChildren newNode Nothing (State model) tree


setActiveNodeWithChildrenHelper : List Int -> node -> Maybe (List node) -> Tree.Tree node -> Tree.Tree node
setActiveNodeWithChildrenHelper active newNode newChildren tree =
    let
        { flat } =
            computeTree Settings.defaults tree
    in
    -- Handle special case when the tree is completely empty
    -- and a new node is added at the root.
    if tree == Tree.Empty && active == [] then
        Tree.Node newNode []

    else
        let
            node =
                flat
                    |> List.filter (\( path, _ ) -> active == path)
                    |> List.head
                    |> Maybe.map Tuple.second
        in
        case node of
            Just (Just _) ->
                TreeExtra.updateAtWithChildren active newNode newChildren tree

            Just Nothing ->
                TreeExtra.insert (List.take (List.length active - 1) active) (Just newNode) tree

            _ ->
                -- Impossible state
                tree


{-| Sets the active node with the option to also set its children. The existing children will be discarded along with their children.
-}
setActiveNodeWithChildren : node -> Maybe (List node) -> State node -> Tree.Tree node -> Tree.Tree node
setActiveNodeWithChildren newNode newChildren (State model) tree =
    model.active
        |> Maybe.map
            (\active ->
                setActiveNodeWithChildrenHelper active newNode newChildren tree
            )
        |> Maybe.withDefault tree


{-| Delete the active node from a tree, including all of its children. If a placeholder is active, this method does nothing.
-}
deleteActiveNode : State node -> Tree.Tree node -> ( State node, Tree.Tree node )
deleteActiveNode (State model) tree =
    ( State
        { model
            | active = Nothing
        }
    , model.active
        |> Maybe.map
            (\active ->
                TreeExtra.delete active
                    tree
            )
        |> Maybe.withDefault tree
    )


{-| Module messages
-}
type Msg
    = NodeMouseDown TreeNodePath Float Float
    | NodeMouseUp Float Float
    | NodeMouseEnter TreeNodePath
    | NodeMouseLeave TreeNodePath
    | CanvasMouseMove Float Float
    | CanvasMouseDown Float Float
    | CanvasMouseUp Float Float
    | CanvasMouseLeave
    | NoOp


resolveDrop : Settings.Settings node -> State node -> Tree.Tree node -> Tree.Tree node
resolveDrop settings (State model) tree =
    let
        { flat } =
            computeTree settings tree

        stuff =
            Drag.state model.drag
                |> Maybe.andThen
                    (\( path, dragOffset ) ->
                        Maybe.map (\justPath -> ( justPath, dragOffset )) path
                    )
                |> Maybe.andThen
                    (\( path, dragOffset ) ->
                        getDropTarget settings path dragOffset tree
                            |> Maybe.map (\dropTargetPath -> ( path, dropTargetPath ))
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
                    |> Maybe.map (\_ -> TreeExtra.swap path dropTargetPath tree)
                    |> Maybe.withDefault
                        -- If the drop target is a placeholder, first add an Empty node in the original tree
                        -- so the swap method actually finds a node.
                        (TreeExtra.insert (List.take (List.length dropTargetPath - 1) dropTargetPath) Nothing tree
                            |> TreeExtra.swap path dropTargetPath
                            |> TreeExtra.removeEmpties
                        )
            )
        |> Maybe.withDefault tree


{-| Update function handling changes in the model.
-}
update : Settings.Settings node -> Msg -> State node -> Tree.Tree node -> ( State node, Tree.Tree node )
update settings msg (State model) tree =
    case msg of
        NodeMouseDown path x y ->
            let
                active_ =
                    if not settings.isDragAndDropEnabled then
                        Just path

                    else
                        Nothing
            in
            ( State
                { model
                    | drag =
                        if not settings.isDragAndDropEnabled then
                            Drag.init

                        else
                            Drag.start (Just path) x y
                    , active =
                        if active_ == Nothing && (settings.isSturdyMode == True || not settings.isDragAndDropEnabled) then
                            model.active

                        else
                            active_
                }
            , tree
            )

        NodeMouseUp _ _ ->
            let
                active_ =
                    if not settings.isDragAndDropEnabled then
                        model.active

                    else
                        Drag.state model.drag
                            |> Maybe.andThen
                                (\( path, ( offsetX, offsetY ) ) ->
                                    case path of
                                        Just _ ->
                                            -- If the node was dragged far enough already, it is not activated
                                            -- This case also protects from reading a slightly sloppy click as a drag
                                            if abs offsetX + abs offsetY > 20 then
                                                Nothing

                                            else
                                                path

                                        Nothing ->
                                            Nothing
                                )

                { flat } =
                    computeTree settings tree

                newTree =
                    case active_ of
                        Just path ->
                            case Dict.get path (Dict.fromList flat) of
                                Just (Just _) ->
                                    tree

                                -- A placeholder has been clicked. Add new node to the tree
                                Just Nothing ->
                                    case settings.defaultNode of
                                        Just defaultNode ->
                                            setActiveNodeWithChildrenHelper path defaultNode Nothing tree

                                        Nothing ->
                                            tree

                                Nothing ->
                                    tree

                        Nothing ->
                            resolveDrop settings (State model) tree

                { layout } =
                    computeTree settings newTree

                newPanOffset =
                    active_
                        |> Maybe.andThen
                            (\justActive ->
                                nodeGeometry settings justActive layout
                                    |> Maybe.map .center
                                    |> Maybe.map
                                        (\( cx, cy ) ->
                                            [ settings.centerOffset
                                            , ( settings.canvasWidth / 2
                                              , settings.canvasHeight / 2
                                              )
                                            , ( -cx, -settings.nodeHeight / 2 - cy )
                                            ]
                                                |> List.foldl Utils.addFloatTuples ( 0, 0 )
                                        )
                            )
            in
            ( State
                { model
                    | drag =
                        Drag.init
                    , panOffset =
                        newPanOffset |> Maybe.withDefault model.panOffset
                    , active =
                        if active_ == Nothing && settings.isSturdyMode == True then
                            model.active

                        else
                            active_
                }
            , newTree
            )

        NodeMouseEnter path ->
            ( State
                { model
                    | hovered =
                        Just path
                }
            , tree
            )

        NodeMouseLeave path ->
            ( State
                { model
                    | hovered =
                        if model.hovered == Just path then
                            Nothing

                        else
                            model.hovered
                }
            , tree
            )

        CanvasMouseMove xm ym ->
            ( State
                { model
                    | drag =
                        Drag.move xm ym model.drag
                }
            , tree
            )

        CanvasMouseDown x y ->
            ( State
                { model
                    | drag = Drag.start Nothing x y
                }
            , tree
            )

        CanvasMouseUp _ _ ->
            let
                active_ =
                    Drag.state model.drag
                        |> Maybe.map
                            (\( _, ( dragX, dragY ) ) ->
                                if (abs dragX + abs dragY) < 21 then
                                    Nothing

                                else
                                    model.active
                            )
                        |> Maybe.withDefault Nothing
            in
            ( State
                { model
                    | drag = Drag.init
                    , panOffset =
                        Drag.state model.drag
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
                        if active_ == Nothing && settings.isSturdyMode == True then
                            model.active

                        else
                            active_
                }
            , tree
            )

        CanvasMouseLeave ->
            ( State { model | drag = Drag.init }
            , tree
            )

        NoOp ->
            ( State model
            , tree
            )


getDropTarget :
    Settings.Settings node
    -> TreeNodePath
    -> ( Float, Float )
    -> Tree.Tree node
    -> Maybe TreeNodePath
getDropTarget settings path ( dragX, dragY ) tree =
    let
        { flat, layout } =
            computeTree settings tree

        ( x0, y0 ) =
            nodeGeometry settings path layout
                |> Maybe.map .center
                |> Maybe.withDefault ( 0, 0 )

        ( x, y ) =
            ( x0 + dragX
            , y0 + dragY
            )
    in
    flat
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


nodeGeometry : Settings.Settings node -> List Int -> TreeExtra.Layout -> Maybe NodeGeometry
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
                    ( settings.canvasWidth / 2 + centerX * (settings.nodeWidth + settings.gutter)
                    , settings.canvasHeight * 0.1 + centerY * (settings.nodeHeight + settings.level)
                    )
                , childCenters =
                    List.map
                        (\childCenter ->
                            ( settings.canvasWidth / 2 + childCenter * (settings.nodeWidth + settings.gutter)
                            , settings.canvasHeight * 0.1 + (centerY + 1) * (settings.nodeHeight + settings.level)
                            )
                        )
                        childCenters
                }
            )


{-| View function for an individual node, depending on its [context](Context), and its value. This value is expressed as a maybe because the node may contain an `insert new node`-type placeholder.
-}
type alias NodeView node =
    Context node -> Maybe node -> Html.Html Msg


isSibling : List Int -> List Int -> Bool
isSibling nodePath1 nodePath2 =
    nodePath1
        /= nodePath2
        && List.length nodePath1
        == List.length nodePath2
        && List.take (List.length nodePath2 - 1) nodePath2
        == List.take (List.length nodePath1 - 1) nodePath1


viewContext : Settings.Settings node -> State node -> Tree.Tree node -> List Int -> Context node
viewContext settings (State model) tree path =
    let
        { flat } =
            computeTree settings tree

        isDropTarget =
            Drag.state model.drag
                |> Maybe.map
                    (\( draggedPath, offset ) ->
                        case draggedPath of
                            Just justDraggedPath ->
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


nodeDragInfo : List Int -> State node -> ( Bool, ( Float, Float ) )
nodeDragInfo path (State model) =
    let
        modelIsDragging =
            Drag.state model.drag /= Nothing

        dragState =
            Drag.state model.drag
    in
    if modelIsDragging then
        dragState
            |> Maybe.map
                (\( draggedPath, offset ) ->
                    case draggedPath of
                        Just justDraggedPath ->
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


{-| The editor's view function, taking the following arguments:

  - [NodeView](#NodeView): view function for an individual node.
  - a list of html attributes for the container element.
  - the editor's [model](#State).

-}
view :
    List (Html.Attribute Msg)
    ->
        { nodeView : NodeView node
        , tree : Tree.Tree node
        , state : State node
        , toMsg : (State node -> Tree.Tree node -> ( State node, Tree.Tree node )) -> msg
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

        { flat, layout } =
            computeTree settings config.tree

        nodeBaseStyle =
            Styles.nodeBase settings

        coordStyle =
            Styles.coordinate settings

        dragState =
            Drag.state model.drag

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
            (Decode.map2 CanvasMouseMove
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
         , on "mouseleave" (Decode.succeed CanvasMouseLeave)
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
        [ div
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
                    nodeGeometry settings path layout
                        |> Maybe.map
                            (\{ center, childCenters } ->
                                let
                                    ( x, y ) =
                                        center

                                    ( isDragged, ( xDrag, yDrag ) ) =
                                        nodeDragInfo path (State model)

                                    xWithDrag =
                                        x + xDrag

                                    yWithDrag =
                                        y + yDrag

                                    nodeViewContext =
                                        viewContext settings (State model) config.tree path
                                in
                                [ div
                                    ((nodeBaseStyle
                                        ++ coordStyle ( xWithDrag, yWithDrag )
                                        ++ (if isDragged then
                                                [ ( "z-index", "100" )
                                                , ( "cursor", "move" )
                                                ]

                                            else
                                                []
                                           )
                                        |> List.map (\( property, value ) -> style property value)
                                     )
                                        ++ [ stopPropagationOn "mousedown"
                                                (Decode.map2 (NodeMouseDown path)
                                                    (Decode.field "screenX" Decode.float)
                                                    (Decode.field "screenY" Decode.float)
                                                    |> Decode.map (\msg -> ( msg, True ))
                                                )
                                           , stopPropagationOn "mouseup"
                                                (Decode.map2 NodeMouseUp
                                                    (Decode.field "screenX" Decode.float)
                                                    (Decode.field "screenY" Decode.float)
                                                    |> Decode.map (\msg -> ( msg, True ))
                                                )
                                           , on "mouseenter" (Decode.succeed (NodeMouseEnter path))
                                           , on "mouseleave" (Decode.succeed (NodeMouseLeave path))
                                           ]
                                    )
                                    [ nodeView nodeViewContext node
                                    ]
                                , if node == Nothing then
                                    text ""

                                  else
                                    Views.NodeConnectors.view
                                        settings
                                        1.0
                                        ( xDrag, yDrag )
                                        center
                                        childCenters
                                        |> Html.map (always NoOp)
                                ]
                                    ++ (if isDragged && (abs xDrag + abs yDrag > 60) then
                                            div
                                                ((nodeBaseStyle
                                                    ++ coordStyle ( x, y )
                                                    |> List.map (\( property, value ) -> style property value)
                                                 )
                                                    ++ [ style "background-color" "rgba(0, 0, 0, 0.05)"
                                                       ]
                                                )
                                                []
                                                :: (if node == Nothing then
                                                        []

                                                    else
                                                        [ Views.NodeConnectors.view
                                                            settings
                                                            0.3
                                                            ( 0, 0 )
                                                            center
                                                            childCenters
                                                            |> Html.map (always NoOp)
                                                        ]
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
        |> Html.map
            (\msg ->
                config.toMsg
                    (\prevState prevTree ->
                        update settings msg prevState prevTree
                    )
            )


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
