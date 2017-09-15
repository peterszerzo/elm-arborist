module Treditor
    exposing
        ( Model
        , Config
        , Msg
        , init
        , update
        , view
        , tree
        , isNew
        , setNew
        , active
        , setActive
        )

import Json.Decode as Decode
import Html exposing (Html, Attribute, node, div, text, p, h1, h3, label, input)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput, on, onWithOptions)
import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, stroke, strokeWidth)
import Html.Events exposing (onClick)
import Data.BinaryTree as Tree
import MultiDrag as MultiDrag
import Colors exposing (..)
import Utils
import Views.NodeConnectors


-- Model


type alias NodeId =
    String


type alias Config item =
    { toId : item -> NodeId
    , view : item -> String
    , layout :
        { width : Float
        , height : Float
        , levelGap : Float
        }
    }


type alias TreeContext item =
    { tree : Tree.Tree item
    , active : Maybe NodeId
    , new : Maybe EmptyLeaf
    , dropTarget : Maybe NodeId
    , dragState : Maybe ( NodeId, ( Float, Float ) )
    }


type Model item
    = Model
        { tree : Tree.Tree item
        , new : Maybe EmptyLeaf
        , active : Maybe NodeId
        , drag : MultiDrag.Drag NodeId
        , displayRoot : Maybe NodeId
        }


type alias EmptyLeaf =
    { parentId : NodeId
    , isLeft : Bool
    }


type alias NodeGeometry =
    { position : ( Float, Float )
    , childOffset : Float
    }


tree : Model item -> Tree.Tree item
tree (Model { tree }) =
    tree


isNew : Model item -> Bool
isNew (Model { new }) =
    new /= Nothing


setNew : Config item -> item -> Model item -> Model item
setNew config item (Model model) =
    case model.new of
        Just new ->
            let
                newTree =
                    Tree.insert (\item -> config.toId item == new.parentId) new.isLeft (Tree.singleton item) model.tree
            in
                Model
                    { model
                        | tree =
                            if Tree.uniqueIds config.toId newTree then
                                newTree
                            else
                                model.tree
                    }

        Nothing ->
            Model model


active : Config item -> Model item -> Maybe item
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
    Config item
    -> String
    -> ( Float, Float )
    -> Tree.Tree item
    -> Maybe String
getDropTarget config id ( dragX, dragY ) tree =
    let
        ( x0, y0 ) =
            nodeGeometry config id tree
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
                        nodeGeometry config (config.toId item) tree
                            |> Maybe.map .position
                            |> Maybe.withDefault ( 0, 0 )
                in
                    (config.toId item) /= id && (abs (x - xo) < config.layout.width) && (abs (y - yo) < config.layout.height)
            )
            tree
            |> List.head
            |> Maybe.map config.toId


init : Tree.Tree item -> Model item
init tree =
    Model
        { tree = tree
        , active = Nothing
        , new = Nothing
        , drag = (MultiDrag.init)
        , displayRoot = Nothing
        }


childNodeHorizontalOffset : Float -> Int -> Float
childNodeHorizontalOffset width depth =
    if depth == 0 then
        width + 2 * 10
    else
        width / 2 + 10


nodeGeometry_ : Config item -> Int -> NodeId -> Tree.Tree item -> Maybe NodeGeometry
nodeGeometry_ config depth id tree =
    let
        childOffset =
            childNodeHorizontalOffset config.layout.width depth
    in
        case tree of
            Tree.Empty ->
                Nothing

            Tree.Node item left right ->
                if config.toId item == id then
                    Just
                        { position = ( 0, 0 )
                        , childOffset = childOffset
                        }
                else
                    (let
                        ( leftGeometry, leftOffsetPt ) =
                            ( nodeGeometry_ config (depth + 1) id left, ( -childOffset, (config.layout.height + 36) ) )

                        ( rightGeometry, rightOffsetPt ) =
                            ( nodeGeometry_ config (depth + 1) id right, ( childOffset, (config.layout.height + 36) ) )
                     in
                        case ( leftGeometry, rightGeometry ) of
                            ( Just leftGeometry, _ ) ->
                                Just
                                    { position = Utils.addFloatTuples leftOffsetPt leftGeometry.position
                                    , childOffset = leftGeometry.childOffset
                                    }

                            ( _, Just rightGeometry ) ->
                                Just
                                    { position = Utils.addFloatTuples rightOffsetPt rightGeometry.position
                                    , childOffset = rightGeometry.childOffset
                                    }

                            ( _, _ ) ->
                                Nothing
                    )


nodeGeometry : Config item -> NodeId -> Tree.Tree item -> Maybe NodeGeometry
nodeGeometry config =
    nodeGeometry_ config 0


setActive : item -> Msg item
setActive =
    SetActive



-- Msg


type Msg item
    = Activate String
    | Deactivate
    | SetActive item
    | SetNew String Bool
    | MouseMove Float Float
    | MouseDown String Float Float
    | MouseUp Float Float



-- Update


update : Config item -> Msg item -> Model item -> Model item
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

        SetNew toId isLeft ->
            Model
                { model
                    | new =
                        Just
                            { parentId = toId
                            , isLeft = isLeft
                            }
                }

        Deactivate ->
            Model { model | active = Nothing }

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
    Config item
    -> TreeContext item
    -> Maybe { parent : NodeId, isLeft : Bool }
    -> Tree.Tree item
    -> List (Html (Msg item))
viewTree_ config context parent tree =
    case tree of
        Tree.Empty ->
            case parent of
                Just { parent, isLeft } ->
                    nodeGeometry config parent context.tree
                        |> Maybe.map
                            ((\{ position, childOffset } ->
                                let
                                    ( x, y ) =
                                        position
                                in
                                    [ p
                                        [ style <|
                                            (context.new
                                                |> Maybe.map
                                                    (\new ->
                                                        if new.parentId == parent && new.isLeft == isLeft then
                                                            highlightedPlaceholderNodeStyle
                                                        else
                                                            placeholderNodeStyle
                                                    )
                                                |> Maybe.withDefault placeholderNodeStyle
                                                |> (\fn -> fn config.layout.width config.layout.height)
                                            )
                                                ++ (coordinateStyle
                                                        config.layout.width
                                                        (x
                                                            + (if isLeft then
                                                                -childOffset
                                                               else
                                                                childOffset
                                                              )
                                                        )
                                                        (y + (config.layout.height + 36))
                                                   )
                                        , onClick (SetNew parent isLeft)
                                        ]
                                        []
                                    ]
                             )
                            )
                        |> Maybe.withDefault []

                Nothing ->
                    []

        Tree.Node item left right ->
            nodeGeometry config (config.toId item) context.tree
                |> Maybe.map
                    (\{ position, childOffset } ->
                        let
                            ( x, y ) =
                                position

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
                        in
                            [ p
                                [ style <|
                                    nodeStyle config.layout.width config.layout.height
                                        ++ (coordinateStyle config.layout.width (x + dragOffsetX) (y + dragOffsetY))
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
                            , svg
                                [ width (toString (childOffset * 2))
                                , height "32"
                                , viewBox <| "0 0 " ++ (toString (childOffset * 2)) ++ " 32"
                                , style <|
                                    [ ( "position", "absolute" ) ]
                                        ++ (coordinateStyle config.layout.width (x - childOffset + (config.layout.width / 2)) (y + (config.layout.height + 15)))
                                ]
                                (Views.NodeConnectors.view
                                    (childOffset * 2)
                                    (config.layout.height + 11)
                                )
                            ]
                                ++ (if isDragged then
                                        [ p
                                            [ style <|
                                                (placeholderNodeStyle config.layout.width config.layout.height)
                                                    ++ coordinateStyle config.layout.width x y
                                            ]
                                            []
                                        ]
                                    else
                                        []
                                   )
                                ++ (viewTree_ config context (Just { parent = config.toId item, isLeft = True }) left)
                                ++ (viewTree_ config context (Just { parent = config.toId item, isLeft = False }) right)
                    )
                |> Maybe.withDefault []


viewTree : Config item -> TreeContext item -> Tree.Tree item -> List (Html (Msg item))
viewTree config context tree =
    viewTree_ config context Nothing tree


view : Config item -> List (Attribute (Msg item)) -> Model item -> Html (Msg item)
view config attrs (Model model) =
    let
        activeItem =
            (active config) (Model model)

        treeContext =
            { tree = model.tree
            , active =
                activeItem
                    |> Maybe.map config.toId
            , new = model.new
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
            viewTree config treeContext model.tree


coordinateStyle : Float -> Float -> Float -> List ( String, String )
coordinateStyle width x y =
    [ ( "left", "calc(50% + " ++ (toString (x - (width / 2))) ++ "px)" )
    , ( "top", "calc(10% + " ++ (toString y) ++ "px)" )
    ]


nodeBaseStyle : Float -> Float -> List ( String, String )
nodeBaseStyle width height =
    [ ( "width", (toString width) ++ "px" )
    , ( "height", "auto" )
    , ( "min-height", (toString height) ++ "px" )
    , ( "text-align", "center" )
    , ( "position", "absolute" )
    , ( "padding", "3px 0" )
    , ( "user-select", "none" )
    , ( "border-radius", "4px" )
    , ( "cursor", "pointer" )
    ]


nodeStyle : Float -> Float -> List ( String, String )
nodeStyle width height =
    nodeBaseStyle width height
        ++ [ ( "background", "rgba(0, 0, 0, 0.8)" )
           , ( "color", "white" )
           ]


placeholderNodeStyle : Float -> Float -> List ( String, String )
placeholderNodeStyle width height =
    nodeBaseStyle width height ++ [ ( "border", "2px dashed #DDDDDD" ) ]


highlightedPlaceholderNodeStyle : Float -> Float -> List ( String, String )
highlightedPlaceholderNodeStyle width height =
    nodeBaseStyle width height ++ [ ( "border", "2px dashed #333333" ) ]
