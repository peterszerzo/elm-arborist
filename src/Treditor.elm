module Treditor
    exposing
        ( Model
        , Msg(SetActive)
        , init
        , update
        , view
        , tree
        , isNew
        , setNew
        , delete
        , active
        )

import Json.Decode as Decode
import Html exposing (Html, Attribute, node, div, text, p, h1, h3, label, input, button)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput, on, onWithOptions)
import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, stroke, strokeWidth)
import Html.Events exposing (onClick)
import Data.Tree as Tree
import MultiDrag as MultiDrag
import Colors exposing (..)
import Utils
import Views.Styles as Styles
import Views.NodeConnectors
import Treditor.Config as Config
import Geometry


-- Types


type alias NodeId =
    String


type alias TreeContext item =
    { tree : Tree.Tree item
    , active : Maybe NodeId
    , focus : Maybe NodeId
    , new : Maybe NodeId
    , dropTarget : Maybe NodeId
    , dragState : Maybe ( NodeId, ( Float, Float ) )
    }


type alias TreeDrawLocalContext =
    { parent : NodeId
    , parentDepth : Int
    , index : Int
    , parentDragOffset : ( Float, Float )
    }



-- Model


type Model item
    = Model
        { tree : Tree.Tree item
        , new : Maybe NodeId
        , active : Maybe NodeId
        , focus : Maybe NodeId
        , drag : MultiDrag.Drag NodeId
        , displayRoot : Maybe NodeId
        }


init : Tree.Tree item -> Model item
init tree =
    Model
        { tree = tree
        , active = Nothing
        , new = Nothing
        , focus = Nothing
        , drag = (MultiDrag.init)
        , displayRoot = Nothing
        }


tree : Model item -> Tree.Tree item
tree (Model { tree }) =
    tree


isNew : Model item -> Bool
isNew (Model { new }) =
    new /= Nothing


delete : Config.Config item -> NodeId -> Model item -> Model item
delete config itemId (Model model) =
    Model { model | tree = Tree.remove (\item -> config.toId item == itemId) model.tree }


setNew : Config.Config item -> item -> Model item -> Model item
setNew config item (Model model) =
    case model.new of
        Just new ->
            let
                newTree =
                    Tree.insert (\item -> config.toId item == new)
                        (Tree.singleton item)
                        model.tree
            in
                Model
                    { model | tree = newTree }

        Nothing ->
            Model model


active : Config.Config item -> Model item -> Maybe item
active config (Model { tree, active }) =
    active
        |> Maybe.andThen
            (\active ->
                Tree.find
                    (\item -> config.toId item == active)
                    tree
                    |> List.head
            )


getDropTarget :
    Config.Config item
    -> NodeId
    -> ( Float, Float )
    -> Tree.Tree item
    -> Maybe String
getDropTarget config id ( dragX, dragY ) tree =
    let
        ( x0, y0 ) =
            Geometry.nodeGeometry config id tree
                |> Maybe.map .position
                |> Maybe.withDefault ( 0, 0 )

        ( x, y ) =
            ( x0 + dragX
            , y0 + dragY
            )
    in
        Tree.find
            (\item ->
                let
                    ( xo, yo ) =
                        Geometry.nodeGeometry config (config.toId item) tree
                            |> Maybe.map .position
                            |> Maybe.withDefault ( 0, 0 )
                in
                    (config.toId item) /= id && (abs (x - xo) < config.layout.width) && (abs (y - yo) < config.layout.height)
            )
            tree
            |> List.head
            |> Maybe.map config.toId



-- Msg


type Msg item
    = Activate NodeId
    | Deactivate
    | SetActive item
    | SetNew NodeId
    | SetFocus (Maybe NodeId)
    | MouseMove Float Float
    | MouseDown NodeId Float Float
    | MouseUp Float Float



-- Update


update : Config.Config item -> Msg item -> Model item -> Model item
update config msg (Model model) =
    case msg of
        Activate id ->
            Model
                { model
                    | active =
                        if model.active == Just id then
                            Nothing
                        else
                            Just id
                    , new = Nothing
                }

        SetActive newItem ->
            Model
                { model
                    | tree =
                        model.active
                            |> Maybe.map
                                (\active ->
                                    Tree.map
                                        (\item ->
                                            if config.toId item == active then
                                                newItem
                                            else
                                                item
                                        )
                                        model.tree
                                )
                            |> Maybe.withDefault model.tree
                }

        SetFocus focus ->
            let
                rootId =
                    case model.tree of
                        Tree.Empty ->
                            Nothing

                        Tree.Node item _ ->
                            Just <| config.toId item
            in
                Model
                    { model
                        | focus =
                            -- Make explicit in the state that if the root is focused, nothing is focused (to avoid swapping bugs).
                            if focus == rootId then
                                Nothing
                            else
                                focus
                    }

        SetNew nodeId ->
            Model
                { model | new = Just nodeId }

        Deactivate ->
            Model { model | active = Nothing, new = Nothing }

        MouseDown id x y ->
            Model
                { model
                    | drag =
                        MultiDrag.start id x y
                }

        MouseMove xm ym ->
            Model
                { model
                    | drag =
                        MultiDrag.move xm ym model.drag
                }

        MouseUp x y ->
            let
                newTree =
                    MultiDrag.state model.drag
                        |> Maybe.map
                            (\( id, dragOffset ) ->
                                let
                                    dropTarget =
                                        getDropTarget config id dragOffset model.tree
                                in
                                    Tree.swapOne
                                        (\item -> (config.toId item) == id)
                                        (\item -> dropTarget == (Just (config.toId item)))
                                        model.tree
                            )
                        |> Maybe.withDefault model.tree
            in
                Model
                    { model
                        | drag = MultiDrag.init
                        , active = Nothing
                        , tree = newTree
                    }



-- View


viewTree_ :
    Config.Config item
    -> TreeContext item
    -> Maybe TreeDrawLocalContext
    -> Tree.Tree item
    -> List (Html (Msg item))
viewTree_ config context localContext tree =
    let
        nodeBaseStyle =
            Styles.nodeBase config.layout.width config.layout.height
    in
        case tree of
            Tree.Empty ->
                [ text "The tree is empty." ]

            Tree.Node item children ->
                let
                    currentId =
                        config.toId item
                in
                    Geometry.nodeGeometry config currentId context.tree
                        |> Maybe.map
                            (\{ position, childSpan } ->
                                let
                                    ( isDragged, ( dragOffsetX, dragOffsetY ) ) =
                                        context.dragState
                                            |> Maybe.map
                                                (\( id, offset ) ->
                                                    if id == (config.toId item) then
                                                        ( True, offset )
                                                    else
                                                        ( False, ( 0, 0 ) )
                                                )
                                            |> Maybe.withDefault ( False, ( 0, 0 ) )

                                    parentDragOffsetWithDefault =
                                        localContext
                                            |> Maybe.map .parentDragOffset
                                            |> Maybe.withDefault ( 0, 0 )

                                    ( x, y ) =
                                        position
                                            |> Utils.addFloatTuples ( dragOffsetX, dragOffsetY )
                                            |> Utils.addFloatTuples parentDragOffsetWithDefault

                                    coordStyle =
                                        Styles.coordinate config.layout.width

                                    ( buttonIcon, buttonMsg ) =
                                        case ( localContext, context.focus ) of
                                            ( Just { parent }, Nothing ) ->
                                                ( "🔎", Just currentId |> SetFocus )

                                            ( Just { parent }, Just focus ) ->
                                                if (focus == currentId) then
                                                    (( "☝️", Just parent |> SetFocus ))
                                                else
                                                    ( "🔎", Just currentId |> SetFocus )

                                            ( Nothing, _ ) ->
                                                ( "☝️", Nothing |> SetFocus )
                                in
                                    [ p
                                        [ style <|
                                            nodeBaseStyle
                                                ++ Styles.regularNode
                                                ++ (coordStyle x y)
                                                ++ [ ( "background-color"
                                                     , if (context.dropTarget == Just (config.toId item)) then
                                                        red
                                                       else if context.active == Just (config.toId item) then
                                                        blue
                                                       else
                                                        gray
                                                     )
                                                   ]
                                                ++ [ ( "transition"
                                                     , if context.dropTarget == Just (config.toId item) then
                                                        "background 0.3s"
                                                       else
                                                        "none"
                                                     )
                                                   ]
                                                ++ (if isDragged then
                                                        [ ( "z-index", "100" ) ]
                                                    else
                                                        []
                                                   )
                                        , Utils.onClickStopPropagation (Activate (config.toId item))
                                        , on "mousedown"
                                            (Decode.map2 (MouseDown (config.toId item))
                                                (Decode.field "screenX" Decode.float)
                                                (Decode.field "screenY" Decode.float)
                                            )
                                        , on "mouseup"
                                            (Decode.map2 MouseUp
                                                (Decode.field "screenX" Decode.float)
                                                (Decode.field "screenY" Decode.float)
                                            )
                                        ]
                                        [ text (config.view item) ]
                                    , if localContext == Nothing then
                                        div [] []
                                      else
                                        button
                                            [ style <|
                                                [ ( "position", "absolute" )
                                                , ( "font-size", "8px" )
                                                , ( "transform", "translate3d(-50%, -100%, 0)" )
                                                ]
                                                    ++ coordStyle (x + config.layout.width / 2) (y - 5)
                                            , Utils.onClickStopPropagation buttonMsg
                                            ]
                                            [ text buttonIcon
                                            ]
                                    , svg
                                        [ width (toString (childSpan * 2))
                                        , height (toString config.layout.verticalGap)
                                        , viewBox <| "0 0 " ++ (toString (childSpan * 2)) ++ " " ++ (toString config.layout.verticalGap)
                                        , style <|
                                            [ ( "position", "absolute" ) ]
                                                ++ (coordStyle (x - childSpan / 2 + config.layout.width / 2) (y + config.layout.height))
                                        ]
                                        (Views.NodeConnectors.view (List.length children)
                                            childSpan
                                            (config.layout.verticalGap)
                                        )
                                    ]
                                        ++ (if isDragged then
                                                [ p
                                                    [ style <|
                                                        nodeBaseStyle
                                                            ++ Styles.placeholderNode
                                                            ++ (coordStyle x y)
                                                    ]
                                                    []
                                                ]
                                            else
                                                []
                                           )
                                        ++ (List.indexedMap
                                                (\index child ->
                                                    (viewTree_ config
                                                        context
                                                        (Just
                                                            { parent = config.toId item
                                                            , parentDepth =
                                                                localContext
                                                                    |> Maybe.map (\p -> p.parentDepth + 1)
                                                                    |> Maybe.withDefault 0
                                                            , index = index
                                                            , parentDragOffset =
                                                                if isDragged then
                                                                    ( dragOffsetX, dragOffsetY )
                                                                else
                                                                    parentDragOffsetWithDefault
                                                            }
                                                        )
                                                        child
                                                    )
                                                )
                                                children
                                                |> List.foldl (++) []
                                           )
                            )
                        |> Maybe.withDefault []


viewTree : Config.Config item -> TreeContext item -> Maybe item -> List (Html (Msg item))
viewTree config context item =
    viewTree_ config
        context
        (item
            |> Maybe.map
                (\i ->
                    { parent = config.toId i
                    , parentDepth = 0
                    , index = 0
                    , parentDragOffset = ( 0, 0 )
                    }
                )
        )
        context.tree


view : Config.Config item -> List (Attribute (Msg item)) -> Model item -> Html (Msg item)
view config attrs (Model model) =
    let
        ( subtree, parent ) =
            model.focus
                |> Maybe.andThen (\id -> Tree.findOneSubtreeWithParent (\item -> config.toId item == id) model.tree)
                |> Maybe.withDefault ( model.tree, Nothing )

        _ =
            subtree
                |> Tree.analyze config.toId
                |> Tree.layout 3

        treeContext =
            { tree = subtree
            , active = model.active
            , new = model.new
            , focus = model.focus
            , dropTarget =
                MultiDrag.state model.drag
                    |> Maybe.andThen
                        (\( id, dragOffset ) ->
                            getDropTarget config id dragOffset model.tree
                        )
            , dragState = MultiDrag.state model.drag
            }
    in
        div
            ([ on "mousemove"
                (Decode.map2 MouseMove
                    (Decode.field "screenX" Decode.float)
                    (Decode.field "screenY" Decode.float)
                )
             , style [ ( "text-align", "center" ) ]
             , onClick Deactivate
             ]
                ++ attrs
            )
        <|
            viewTree config treeContext parent
