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
import Tree as Tree
import MultiDrag as MultiDrag
import Colors exposing (..)


-- Constants


nodeWidth : Float
nodeWidth =
    100


nodeHeight : Float
nodeHeight =
    24



-- Model


type alias Config item =
    { nodeId : item -> String
    , nodeView : item -> String
    }


type alias TreeContext item =
    { tree : Tree.Tree item
    , active : Maybe String
    , new : Maybe EmptyLeaf
    , dropTarget : Maybe String
    , dragState : Maybe ( String, ( Float, Float ) )
    }


type Model item
    = Model
        { tree : Tree.Tree item
        , new : Maybe EmptyLeaf
        , active : Maybe String
        , drag : MultiDrag.Drag String
        }


type alias EmptyLeaf =
    { parentId : String
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
                    Tree.insert (\item -> config.nodeId item == new.parentId) new.isLeft (Tree.singleton item) model.tree
            in
                Model
                    { model
                        | tree =
                            if Tree.uniqueIds config.nodeId newTree then
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
                    (\item -> config.nodeId item == active)
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
                        nodeGeometry config (config.nodeId item) tree
                            |> Maybe.map .position
                            |> Maybe.withDefault ( 0, 0 )
                in
                    (config.nodeId item) /= id && (abs (x - xo) < nodeWidth) && (abs (y - yo) < nodeHeight)
            )
            tree
            |> List.head
            |> Maybe.map config.nodeId


init : Tree.Tree item -> Model item
init tree =
    Model
        { tree = tree
        , active = Nothing
        , new = Nothing
        , drag = (MultiDrag.init)
        }


addFloatTuples : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
addFloatTuples ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


childNodeHorizontalOffset : Int -> Float
childNodeHorizontalOffset depth =
    if depth == 0 then
        nodeWidth + 2 * 10
    else
        nodeWidth / 2 + 10


nodeGeometry_ : Config item -> Int -> String -> Tree.Tree item -> Maybe NodeGeometry
nodeGeometry_ config depth id tree =
    let
        childOffset =
            childNodeHorizontalOffset depth
    in
        case tree of
            Tree.Empty ->
                Nothing

            Tree.Node item left right ->
                if config.nodeId item == id then
                    Just
                        { position = ( 0, 0 )
                        , childOffset = childOffset
                        }
                else
                    (let
                        ( leftGeometry, leftOffsetPt ) =
                            ( nodeGeometry_ config (depth + 1) id left, ( -childOffset, 60 ) )

                        ( rightGeometry, rightOffsetPt ) =
                            ( nodeGeometry_ config (depth + 1) id right, ( childOffset, 60 ) )
                     in
                        case ( leftGeometry, rightGeometry ) of
                            ( Just leftGeometry, _ ) ->
                                Just
                                    { position = addFloatTuples leftOffsetPt leftGeometry.position
                                    , childOffset = leftGeometry.childOffset
                                    }

                            ( _, Just rightGeometry ) ->
                                Just
                                    { position = addFloatTuples rightOffsetPt rightGeometry.position
                                    , childOffset = rightGeometry.childOffset
                                    }

                            ( _, _ ) ->
                                Nothing
                    )


nodeGeometry : Config item -> String -> Tree.Tree item -> Maybe NodeGeometry
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
                                            if config.nodeId item == active then
                                                newItem
                                            else
                                                item
                                        )
                                        model.tree
                                )
                            |> Maybe.withDefault model.tree
                }

        SetNew nodeId isLeft ->
            Model
                { model
                    | new =
                        Just
                            { parentId = nodeId
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
                                        (\item -> (config.nodeId item) == id)
                                        (\item -> dropTarget == (Just (config.nodeId item)))
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


viewLines : Float -> Float -> List (Html msg)
viewLines w h =
    let
        pad =
            1
    in
        [ line
            [ x1 (w / 2 |> toString)
            , y1 (toString pad)
            , x2 (w / 2 |> toString)
            , y2 (h / 2 |> toString)
            , stroke "#CCCCCC"
            , strokeWidth "1"
            ]
            []
        , line
            [ x1 (toString pad)
            , y1 (h / 2 |> toString)
            , x2 (toString (w - pad))
            , y2 (h / 2 |> toString)
            , stroke "#CCCCCC"
            , strokeWidth "1"
            ]
            []
        , line
            [ x1 (toString pad)
            , y1 (h / 2 |> toString)
            , x2 (toString pad)
            , y2 (toString (h - pad))
            , stroke "#CCCCCC"
            , strokeWidth "1"
            ]
            []
        , line
            [ x1 (toString (w - pad))
            , y1 (h / 2 |> toString)
            , x2 (toString (w - pad))
            , y2 (toString (h - pad))
            , stroke "#CCCCCC"
            , strokeWidth "1"
            ]
            []
        ]


viewTree_ :
    Config item
    -> TreeContext item
    -> Maybe { parentNodeId : String, isLeft : Bool }
    -> Tree.Tree item
    -> List (Html (Msg item))
viewTree_ config context parent tree =
    case tree of
        Tree.Empty ->
            case parent of
                Just { parentNodeId, isLeft } ->
                    nodeGeometry config parentNodeId context.tree
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
                                                        if new.parentId == parentNodeId && new.isLeft == isLeft then
                                                            highlightedPlaceholderNodeStyle
                                                        else
                                                            placeholderNodeStyle
                                                    )
                                                |> Maybe.withDefault placeholderNodeStyle
                                            )
                                                ++ (coordinateStyle
                                                        (x
                                                            + (if isLeft then
                                                                -childOffset
                                                               else
                                                                childOffset
                                                              )
                                                        )
                                                        (y + 60)
                                                   )
                                        , onClick (SetNew parentNodeId isLeft)
                                        ]
                                        []
                                    ]
                             )
                            )
                        |> Maybe.withDefault []

                Nothing ->
                    []

        Tree.Node item left right ->
            nodeGeometry config (config.nodeId item) context.tree
                |> Maybe.map
                    (\{ position, childOffset } ->
                        let
                            ( x, y ) =
                                position

                            ( isDragged, ( dragOffsetX, dragOffsetY ) ) =
                                context.dragState
                                    |> Maybe.map
                                        (\( id, offset ) ->
                                            if id == (config.nodeId item) then
                                                ( True, offset )
                                            else
                                                ( False, ( 0, 0 ) )
                                        )
                                    |> Maybe.withDefault ( False, ( 0, 0 ) )
                        in
                            [ p
                                [ style <|
                                    nodeStyle
                                        ++ (coordinateStyle (x + dragOffsetX) (y + dragOffsetY))
                                        ++ [ ( "background-color"
                                             , if (context.dropTarget == Just (config.nodeId item)) then
                                                red
                                               else if context.active == Just (config.nodeId item) then
                                                blue
                                               else
                                                gray
                                             )
                                           ]
                                        ++ [ ( "transition"
                                             , if context.dropTarget == Just (config.nodeId item) then
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
                                , onClickStopPropagation (Activate (config.nodeId item))
                                , on "mousedown"
                                    (Decode.map2 (MouseDown (config.nodeId item))
                                        (Decode.field "screenX" Decode.float)
                                        (Decode.field "screenY" Decode.float)
                                    )
                                , on "mouseup"
                                    (Decode.map2 MouseUp
                                        (Decode.field "screenX" Decode.float)
                                        (Decode.field "screenY" Decode.float)
                                    )
                                ]
                                [ text (config.nodeView item) ]
                            , svg
                                [ width (toString (childOffset * 2))
                                , height "32"
                                , viewBox <| "0 0 " ++ (toString (childOffset * 2)) ++ " 32"
                                , style <|
                                    [ ( "position", "absolute" ) ]
                                        ++ (coordinateStyle (x - childOffset + (nodeWidth / 2)) (y + 39))
                                ]
                                (viewLines
                                    (childOffset * 2)
                                    36
                                )
                            ]
                                ++ (if isDragged then
                                        [ p
                                            [ style <|
                                                placeholderNodeStyle
                                                    ++ coordinateStyle x y
                                            ]
                                            []
                                        ]
                                    else
                                        []
                                   )
                                ++ (viewTree_ config context (Just { parentNodeId = config.nodeId item, isLeft = True }) left)
                                ++ (viewTree_ config context (Just { parentNodeId = config.nodeId item, isLeft = False }) right)
                    )
                |> Maybe.withDefault []


viewTree : Config item -> TreeContext item -> Tree.Tree item -> List (Html (Msg item))
viewTree config context tree =
    viewTree_ config context Nothing tree


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation message =
    let
        config =
            { stopPropagation = True
            , preventDefault = False
            }
    in
        onWithOptions "click" config (Decode.succeed message)


view : Config item -> List (Attribute (Msg item)) -> Model item -> Html (Msg item)
view config attrs (Model model) =
    let
        activeItem =
            (active config) (Model model)

        treeContext =
            { tree = model.tree
            , active =
                activeItem
                    |> Maybe.map config.nodeId
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


coordinateStyle : Float -> Float -> List ( String, String )
coordinateStyle x y =
    [ ( "left", "calc(50% + " ++ (toString (x - (nodeWidth / 2))) ++ "px)" )
    , ( "top", "calc(10% + " ++ (toString y) ++ "px)" )
    ]


nodeBaseStyle : List ( String, String )
nodeBaseStyle =
    [ ( "width", (toString nodeWidth) ++ "px" )
    , ( "height", "auto" )
    , ( "min-height", (toString nodeHeight) ++ "px" )
    , ( "text-align", "center" )
    , ( "position", "absolute" )
    , ( "padding", "3px 0" )
    , ( "user-select", "none" )
    , ( "border-radius", "4px" )
    , ( "cursor", "pointer" )
    ]


nodeStyle : List ( String, String )
nodeStyle =
    nodeBaseStyle
        ++ [ ( "background", "rgba(0, 0, 0, 0.8)" )
           , ( "color", "white" )
           ]


placeholderNodeStyle : List ( String, String )
placeholderNodeStyle =
    nodeBaseStyle ++ [ ( "border", "2px dashed #DDDDDD" ) ]


highlightedPlaceholderNodeStyle : List ( String, String )
highlightedPlaceholderNodeStyle =
    nodeBaseStyle ++ [ ( "border", "2px dashed #333333" ) ]
