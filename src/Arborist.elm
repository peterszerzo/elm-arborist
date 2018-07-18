module Arborist
    exposing
        ( Model
        , Msg
        , subscriptions
        , init
        , initWith
        , applySettings
        , resize
        , reposition
        , deactivate
        , update
        , NodeView
        , StyledNodeView
        , view
        , styledView
        , tree
        , activeNode
        , setActiveNode
        , setActiveNodeWithChildren
        , deleteActiveNode
        , Context
        , NodeState(..)
        )

{-| Drag-and-drop interface to edit, dissect and-rearrange tree structures with arbitrary data sitting in their nodes. Structured as a TEA component defining its own init, update and view, `elm-arborist` allows you to easily initialize a tree editor, keep its state in an opaque model, and access the [edited result at any time](#tree).


# Module setup

@docs Model, Msg, init, initWith, update, NodeView, StyledNodeView, view, styledView, subscriptions


# Configuration

@docs applySettings, resize


# Arborist tree getters and modifiers

@docs tree, activeNode, setActiveNode, setActiveNodeWithChildren, deleteActiveNode


# Display modifiers

@docs reposition, deactivate


# Context

@docs NodeState, Context

-}

import AnimationFrame
import Time
import Dict
import Css exposing (..)
import Html
import Html.Styled exposing (fromUnstyled, toUnstyled, div, text)
import Html.Styled.Keyed
import Html.Styled.Attributes exposing (style, css)
import Html.Styled.Events exposing (on, onWithOptions)
import Json.Decode as Decode
import Arborist.Tree as Tree
import Internal.Tree.Extra as TreeHelpers exposing (TreeNodePath)
import Internal.Settings as Settings
import Drag exposing (Drag)
import Utils
import Views.NodeConnectors
import Views.Styles as Styles


type alias NodeGeometry =
    { center : ( Float, Float )
    , childCenters : List ( Float, Float )
    }


{-| Opaque type for the editor's model, dependent on a node type variable. You can only use this for type annotation - to initialize a new model, see [init](#init).
-}
type Model node
    = Model
        { settings : Settings.Settings node
        , tree : Tree.Tree node
        , active : Maybe TreeNodePath
        , hovered : Maybe TreeNodePath
        , isReceivingSubscriptions : Bool
        , focus : Maybe TreeNodePath
        , drag : Drag (Maybe TreeNodePath)
        , displayRoot : Maybe TreeNodePath
        , panOffset : ( Float, Float )
        , targetPanOffset : Maybe ( Float, Float )
        , isCanvasMouseMoveThrottled : Bool
        }


{-| Initialize model from a [tree](Tree).
-}
init : Tree.Tree node -> Model node
init =
    initWith []


{-| Initialize model from a [tree](Tree), using a list of [settings](Settings).
-}
initWith : List (Settings.Setting node) -> Tree.Tree node -> Model node
initWith settings tree =
    let
        settings_ =
            Settings.apply settings Settings.defaults
    in
        Model
            { settings = settings_
            , tree = tree
            , active =
                if settings_.isSturdyMode && tree /= Tree.Empty then
                    Just []
                else
                    Nothing
            , hovered = Nothing
            , isReceivingSubscriptions = False
            , focus = Nothing
            , drag = Drag.init
            , displayRoot = Nothing
            , panOffset = ( 0, 0 )
            , targetPanOffset = Nothing
            , isCanvasMouseMoveThrottled = False
            }


{-| Apply a new list of settings to the model.
-}
applySettings : List (Settings.Setting node) -> Model node -> Model node
applySettings settings (Model model) =
    Model
        { model
            | settings = Settings.apply settings model.settings
        }


{-| Resize the canvas by passing a new width and a new height. Note that you can reproduce this using [applySettings](#applySettings) as follows:

    resize 600 400 arborist == applySettings [ Settings.canvasWidth 600, Settings.canvasHeight 400 ] arborist

-}
resize : Int -> Int -> Model node -> Model node
resize width height =
    applySettings
        [ Settings.CanvasWidth width
        , Settings.CanvasHeight height
        ]


{-| Restores the original pan position of the tree.
-}
reposition : Model node -> Model node
reposition (Model model) =
    Model
        { model
            | panOffset = ( 0, 0 )
            , targetPanOffset = Nothing
            , drag = Drag.init
        }


{-| Remove active node
-}
deactivate : Model node -> Model node
deactivate (Model model) =
    Model
        { model
            | active = Nothing
        }


computeTree : Model node -> { flat : List ( List Int, Maybe node ), layout : TreeHelpers.Layout }
computeTree (Model model) =
    let
        withPlaceholders =
            case ( model.settings.showPlaceholderLeaves, model.settings.showPlaceholderLeavesAdvanced ) of
                ( _, Just addEmpties ) ->
                    TreeHelpers.addTrailingEmptiesAdvanced addEmpties model.tree

                ( True, _ ) ->
                    TreeHelpers.addTrailingEmpties model.tree

                ( _, _ ) ->
                    model.tree

        flat =
            TreeHelpers.flatten withPlaceholders

        layout =
            withPlaceholders
                |> TreeHelpers.analyze
                |> TreeHelpers.layout
    in
        { layout = layout
        , flat = flat
        }


{-| Returns the current active node as a tuple of `Maybe node` (as the node maybe a placeholder for a new node), as well as some contextual information as a two-field record:

  - `position : ( Float, Float )`: the node's position on the canvas (useful for rendering an edit pop-up).
  - `context`: view context, identical to the one provided in [NodeView](#NodeView).

-}
activeNode : Model node -> Maybe ( Maybe node, { position : ( Float, Float ), context : Context node } )
activeNode (Model model) =
    model.active
        |> Maybe.map
            (\active ->
                let
                    { layout, flat } =
                        computeTree (Model model)

                    geo =
                        nodeGeometry model.settings active layout
                            |> Maybe.map .center
                            |> Maybe.withDefault ( 0, 0 )

                    dragOffset =
                        Drag.state model.drag
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault ( 0, 0 )
                in
                    ( flat
                        |> List.filter (\( path_, item ) -> active == path_)
                        |> List.head
                        |> Maybe.andThen Tuple.second
                    , { position =
                            [ geo
                            , model.panOffset
                            , dragOffset
                            ]
                                |> List.foldl Utils.addFloatTuples ( 0, 0 )
                      , context = viewContext (Model model) active
                      }
                    )
            )


{-| Sets a new node at the active position. This may be adding a completely new node from scratch (in case the current node is a placeholder), or modifying an existing one. Typically, the modification is based off an original value provided by the [activeNodeWithContext](#activeNodeWithContext) method.
-}
setActiveNode : node -> Model node -> Model node
setActiveNode newNode (Model model) =
    setActiveNodeWithChildren newNode Nothing (Model model)


setActiveNodeWithChildrenHelper : List Int -> node -> Maybe (List node) -> Model node -> Model node
setActiveNodeWithChildrenHelper active newNode newChildren (Model model) =
    let
        { flat } =
            computeTree (Model model)

        -- Handle special case when the tree is completely empty
        -- and a new node is added at the root.
        newTree =
            if model.tree == Tree.Empty && active == [] then
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
                        Just (Just node) ->
                            TreeHelpers.updateAtWithChildren active newNode newChildren model.tree

                        Just Nothing ->
                            TreeHelpers.insert (List.take (List.length active - 1) active) (Just newNode) model.tree

                        _ ->
                            -- Impossible state
                            model.tree
    in
        Model { model | tree = newTree }


{-| Sets the active node with the option to also set its children. The existing children will be discarded along with their children.
-}
setActiveNodeWithChildren : node -> Maybe (List node) -> Model node -> Model node
setActiveNodeWithChildren newNode newChildren (Model model) =
    let
        newTree =
            model.active
                |> Maybe.map
                    (\active ->
                        setActiveNodeWithChildrenHelper active newNode newChildren (Model model)
                            |> (\(Model model) -> model.tree)
                    )
                |> Maybe.withDefault model.tree
    in
        Model
            { model
                | tree = newTree
            }


{-| Delete the active node from a tree, including all of its children. If a placeholder is active, this method does nothing.
-}
deleteActiveNode : Model node -> Model node
deleteActiveNode (Model model) =
    Model
        { model
            | tree =
                model.active
                    |> Maybe.map
                        (\active ->
                            TreeHelpers.delete active
                                model.tree
                        )
                    |> Maybe.withDefault model.tree
            , active =
                if model.settings.isSturdyMode then
                    Just [ 0 ]
                else
                    Nothing
        }


{-| Subscriptions responsible for obtaining animation frames used to smoothly center an activated tree node. Using these subscriptions is completely optional - if they aren't wired up in your app, the editor will simply jump to the activated node without an animation. We recommend adding these subscriptions as you familiarize yourself with the package, as it is a significant user experience improvement.
-}
subscriptions : Model node -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ if model.isReceivingSubscriptions && model.targetPanOffset == Nothing then
            Sub.none
          else
            AnimationFrame.times AnimationFrameTick
        , case model.settings.throttleMouseMoves of
            Just interval ->
                Time.every interval MouseMoveThrottleTick

            Nothing ->
                Sub.none
        ]


{-| Access the current state of the tree through this getter (returns structure defined in the `Arborist.Tree` module). The result reflects all changes since it was [initialized](#init).
-}
tree : Model node -> Tree.Tree node
tree (Model { tree }) =
    tree


{-| Module messages
-}
type Msg
    = AnimationFrameTick Time.Time
    | MouseMoveThrottleTick Time.Time
    | NodeMouseDown TreeNodePath Float Float
    | NodeMouseUp Float Float
    | NodeMouseEnter TreeNodePath
    | NodeMouseLeave TreeNodePath
    | CanvasMouseMove Float Float
    | CanvasMouseDown Float Float
    | CanvasMouseUp Float Float
    | CanvasMouseLeave
    | NoOp


moveTowards : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
moveTowards ( x, y ) ( targetX, targetY ) =
    let
        d =
            ((targetX - x) ^ 2 + (targetY - y) ^ 2) ^ 0.5

        dx =
            if targetX == x then
                0
            else
                (d / 10) * (targetX - x) / d

        dy =
            if targetY == y then
                0
            else
                (d / 10) * (targetY - y) / d
    in
        ( x + dx, y + dy )


resolveDrop : Model node -> Tree.Tree node
resolveDrop (Model model) =
    let
        { flat } =
            computeTree (Model model)
    in
        Drag.state model.drag
            |> Maybe.map
                (\( path, dragOffset ) ->
                    path
                        |> Maybe.map
                            (\path ->
                                getDropTarget path dragOffset (Model model)
                                    |> Maybe.map
                                        (\dropTargetPath ->
                                            flat
                                                |> List.filter (\( path, _ ) -> path == dropTargetPath)
                                                |> List.head
                                                |> Maybe.andThen
                                                    (\( path, currentNode ) ->
                                                        case currentNode of
                                                            Just realNode ->
                                                                Just ( path, realNode )

                                                            Nothing ->
                                                                Nothing
                                                    )
                                                |> Maybe.map (\_ -> TreeHelpers.swap path dropTargetPath model.tree)
                                                |> Maybe.withDefault
                                                    -- If the drop target is a placeholder, first add an Empty node in the original tree
                                                    -- so the swap method actually finds a node.
                                                    (TreeHelpers.insert (List.take (List.length dropTargetPath - 1) dropTargetPath) Nothing model.tree
                                                        |> TreeHelpers.swap path dropTargetPath
                                                        |> TreeHelpers.removeEmpties
                                                    )
                                        )
                                    |> Maybe.withDefault model.tree
                            )
                        |> Maybe.withDefault model.tree
                )
            |> Maybe.withDefault model.tree


{-| Update function handling changes in the model.
-}
update : Msg -> Model node -> Model node
update msg (Model model) =
    case msg of
        MouseMoveThrottleTick _ ->
            Model { model | isCanvasMouseMoveThrottled = False }

        -- TODO: account for delayed frames when moving nodes around
        AnimationFrameTick _ ->
            Model
                { model
                    | isReceivingSubscriptions = True
                    , panOffset =
                        model.targetPanOffset
                            |> Maybe.map
                                (\targetPanOffset ->
                                    moveTowards model.panOffset targetPanOffset
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

        NodeMouseDown path x y ->
            let
                active_ =
                    (if not model.settings.isDragAndDropEnabled then
                        Just path
                     else
                        Nothing
                    )
            in
                Model
                    { model
                        | drag =
                            if not model.settings.isDragAndDropEnabled then
                                Drag.init
                            else
                                Drag.start (Just path) x y
                        , active =
                            if active_ == Nothing && model.settings.isSturdyMode == True then
                                model.active
                            else
                                active_
                    }

        NodeMouseUp x y ->
            let
                isPlaceholder =
                    (activeNode (Model model) |> Maybe.map Tuple.first)
                        == (Just Nothing)

                active_ =
                    Drag.state model.drag
                        |> Maybe.andThen
                            (\( path, ( offsetX, offsetY ) ) ->
                                case path of
                                    Just path ->
                                        -- If the node was dragged far enough already, it is not activated
                                        -- This case also protects from reading a slightly sloppy click as a drag
                                        if (abs offsetX + abs offsetY > 20) then
                                            Nothing
                                        else
                                            Just path

                                    Nothing ->
                                        Nothing
                            )

                { flat } =
                    computeTree (Model model)

                newTree =
                    case active_ of
                        Just path ->
                            case Dict.get path (Dict.fromList flat) of
                                Just (Just _) ->
                                    model.tree

                                -- A placeholder has been clicked. Add new node to the tree
                                Just Nothing ->
                                    case model.settings.defaultNode of
                                        Just defaultNode ->
                                            setActiveNodeWithChildrenHelper path defaultNode Nothing (Model model)
                                                |> (\(Model model) -> model.tree)

                                        Nothing ->
                                            model.tree

                                Nothing ->
                                    model.tree

                        Nothing ->
                            resolveDrop (Model model)

                { layout } =
                    computeTree (Model { model | tree = newTree })

                newPanOffset =
                    active_
                        |> Maybe.andThen
                            (\active_ ->
                                nodeGeometry model.settings active_ layout
                                    |> Maybe.map .center
                                    |> Maybe.map
                                        (\( cx, cy ) ->
                                            [ model.settings.centerOffset
                                            , ( model.settings.canvasWidth / 2
                                              , model.settings.canvasHeight / 2
                                              )
                                            , ( -cx, -model.settings.nodeHeight / 2 - cy )
                                            ]
                                                |> List.foldl Utils.addFloatTuples ( 0, 0 )
                                        )
                            )
            in
                Model
                    { model
                        | drag =
                            Drag.init
                        , tree = newTree

                        -- If the client wired up the subscriptions, set the target pan offset to trigger the animation.
                        , targetPanOffset =
                            if model.isReceivingSubscriptions then
                                newPanOffset
                            else
                                Nothing

                        -- Otherwise, center the view directly
                        , panOffset =
                            if model.isReceivingSubscriptions then
                                model.panOffset
                            else
                                newPanOffset |> Maybe.withDefault model.panOffset
                        , active =
                            if active_ == Nothing && model.settings.isSturdyMode == True then
                                model.active
                            else
                                active_
                    }

        NodeMouseEnter path ->
            Model
                { model
                    | hovered =
                        Just path
                }

        NodeMouseLeave path ->
            Model
                { model
                    | hovered =
                        if model.hovered == Just path then
                            Nothing
                        else
                            model.hovered
                }

        CanvasMouseMove xm ym ->
            Model
                { model
                    | drag =
                        Drag.move xm ym model.drag
                    , isCanvasMouseMoveThrottled =
                        if model.isReceivingSubscriptions && model.settings.throttleMouseMoves /= Nothing then
                            True
                        else
                            False
                }

        CanvasMouseDown x y ->
            Model
                { model
                    | drag = Drag.start Nothing x y
                }

        CanvasMouseUp x y ->
            let
                active_ =
                    Drag.state model.drag
                        |> Maybe.map
                            (\( path, ( x, y ) ) ->
                                if (abs x + abs y) < 21 then
                                    Nothing
                                else
                                    model.active
                            )
                        |> Maybe.withDefault Nothing
            in
                Model
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
                            if active_ == Nothing && model.settings.isSturdyMode == True then
                                model.active
                            else
                                active_
                    }

        CanvasMouseLeave ->
            Model { model | drag = Drag.init }

        NoOp ->
            Model model


getDropTarget :
    TreeNodePath
    -> ( Float, Float )
    -> Model node
    -> Maybe TreeNodePath
getDropTarget path ( dragX, dragY ) (Model model) =
    let
        { flat, layout } =
            computeTree (Model model)

        ( x0, y0 ) =
            nodeGeometry model.settings path layout
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
                            nodeGeometry model.settings path_ layout
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
                                , dx < model.settings.nodeWidth
                                , dy < model.settings.nodeHeight
                                ]
                            )
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


nodeGeometry : Settings.Settings node -> List Int -> TreeHelpers.Layout -> Maybe NodeGeometry
nodeGeometry settings path layout =
    layout
        |> Dict.get path
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
                            (\center ->
                                ( settings.canvasWidth / 2 + center * (settings.nodeWidth + settings.gutter)
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


{-| Styled version of [NodeView](#NodeView), using `elm-css`.
-}
type alias StyledNodeView node =
    Context node -> Maybe node -> Html.Styled.Html Msg


viewContext : Model node -> List Int -> Context node
viewContext (Model model) path =
    let
        { flat } =
            computeTree (Model model)

        isDropTarget =
            Drag.state model.drag
                |> Maybe.map
                    (\( draggedPath, offset ) ->
                        case draggedPath of
                            Just draggedPath ->
                                getDropTarget draggedPath offset (Model model)
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
                                    (\node ->
                                        if path /= path_ && List.length path == List.length path_ && List.take (List.length path_ - 1) path_ == List.take (List.length path - 1) path then
                                            Just node
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
                                    (\node ->
                                        if List.length path_ == List.length path + 1 && List.take (List.length path) path_ == path then
                                            Just node
                                        else
                                            Nothing
                                    )
                        )
            , state =
                if (model.active == Just path) then
                    Active
                else if isDropTarget then
                    DropTarget
                else if (model.hovered == Just path) then
                    Hovered
                else
                    Normal
            }
    in
        nodeViewContext


nodeDragInfo : List Int -> Model node -> ( Bool, ( Float, Float ) )
nodeDragInfo path (Model model) =
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
                            Just draggedPath ->
                                if Utils.startsWith draggedPath path then
                                    ( True, offset )
                                else
                                    ( False, ( 0, 0 ) )

                            Nothing ->
                                ( False, ( 0, 0 ) )
                    )
                |> Maybe.withDefault ( False, ( 0, 0 ) )
        else
            ( False, ( 0, 0 ) )


{-| Styled version of [view](#NodeView), using `elm-css`.
-}
view : NodeView node -> List (Html.Attribute Msg) -> Model node -> Html.Html Msg
view nodeView attrs (Model model) =
    styledView
        (\ctx node -> nodeView ctx node |> fromUnstyled)
        (List.map Html.Styled.Attributes.fromUnstyled attrs)
        (Model model)
        |> toUnstyled


{-| The editor's view function, taking the following arguments:

  - [NodeView](#NodeView): view function for an individual node.
  - a list of html attributes for the container element.
  - the editor's [model](#Model).

-}
styledView : StyledNodeView node -> List (Html.Styled.Attribute Msg) -> Model node -> Html.Styled.Html Msg
styledView viewNode attrs (Model model) =
    let
        { flat, layout } =
            computeTree (Model model)

        nodeBaseStyle =
            Styles.nodeBase model.settings

        coordStyle =
            Styles.coordinate model.settings

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
            ((if model.isCanvasMouseMoveThrottled then
                []
              else
                [ on "mousemove"
                    (Decode.map2 CanvasMouseMove
                        (Decode.field "screenX" Decode.float)
                        (Decode.field "screenY" Decode.float)
                    )
                ]
             )
                ++ [ on "mousedown"
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
                   , style
                        [ ( "width", Utils.floatToPxString model.settings.canvasWidth )
                        , ( "height", Utils.floatToPxString model.settings.canvasHeight )
                        , ( "cursor"
                          , if isCanvasDragging then
                                "move"
                            else
                                "auto"
                          )
                        ]
                   , css
                        [ overflow hidden
                        , boxSizing borderBox
                        , position relative
                        ]
                   ]
                ++ attrs
            )
            [ Html.Styled.Keyed.node "div"
                [ css
                    [ width (pct 100)
                    , height (pct 100)
                    , position relative
                    ]
                , style
                    ([ ( "transform"
                       , "translate3d("
                            ++ (Utils.floatToPxString canvasTotalDragOffsetX)
                            ++ ", "
                            ++ (Utils.floatToPxString canvasTotalDragOffsetY)
                            ++ ", 0)"
                       )
                     ]
                        ++ (Styles.throttleTransitionStyles [ "transform" ] model.settings.throttleMouseMoves)
                    )
                ]
              <|
                (List.map
                    (\( path, node ) ->
                        nodeGeometry model.settings path layout
                            |> Maybe.map
                                (\{ center, childCenters } ->
                                    let
                                        ( x, y ) =
                                            center

                                        key =
                                            List.map toString path
                                                |> String.join "-"

                                        ( isDragged, ( xDrag, yDrag ) ) =
                                            nodeDragInfo path (Model model)

                                        xWithDrag =
                                            x + xDrag

                                        yWithDrag =
                                            y + yDrag

                                        nodeViewContext =
                                            viewContext (Model model) path
                                    in
                                        [ ( key
                                          , div
                                                [ style <|
                                                    nodeBaseStyle
                                                        ++ (coordStyle ( xWithDrag, yWithDrag ))
                                                        ++ (Styles.throttleTransitionStyles [ "top", "left" ] model.settings.throttleMouseMoves)
                                                        ++ (if isDragged then
                                                                [ ( "z-index", "100" )
                                                                , ( "cursor", "move" )
                                                                ]
                                                            else
                                                                []
                                                           )
                                                , onWithOptions "mousedown"
                                                    { stopPropagation = True
                                                    , preventDefault = False
                                                    }
                                                    (Decode.map2 (NodeMouseDown path)
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
                                                , on "mouseenter" (Decode.succeed (NodeMouseEnter path))
                                                , on "mouseleave" (Decode.succeed (NodeMouseLeave path))
                                                ]
                                                [ viewNode nodeViewContext node
                                                ]
                                          )
                                        , (if node == Nothing then
                                            ( key ++ "connector2", text "" )
                                           else
                                            ( key ++ "connector2", Views.NodeConnectors.view model.settings 1.0 ( xDrag, yDrag ) center childCenters |> Html.Styled.map (always NoOp) )
                                          )
                                        ]
                                            ++ (if isDragged && (abs xDrag + abs yDrag > 60) then
                                                    [ ( key ++ "shadow"
                                                      , div
                                                            [ style <|
                                                                nodeBaseStyle
                                                                    ++ (coordStyle ( x, y ))
                                                                    ++ (Styles.throttleTransitionStyles [ "top", "left" ] model.settings.throttleMouseMoves)
                                                            , css
                                                                [ backgroundColor <| rgba 0 0 0 0.05
                                                                ]
                                                            ]
                                                            []
                                                      )
                                                    ]
                                                        ++ (if node == Nothing then
                                                                []
                                                            else
                                                                [ ( key ++ "connector1", Views.NodeConnectors.view model.settings 0.3 ( 0, 0 ) center childCenters |> Html.Styled.map (always NoOp) ) ]
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
