module Treditor
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
import Data.Tree as Tree
import MultiDrag as MultiDrag
import Colors exposing (..)
import Utils
import Views.Styles as Styles
import Views.NodeConnectors
import Treditor.Config as Config


-- Types


type alias NodeId =
    String


type alias NodePath =
    List Int


type alias TreeContext item =
    { tree : Tree.Tree item
    , active : Maybe NodePath
    , focus : Maybe NodePath
    , new : Maybe NodePath
    , dropTarget : Maybe NodePath
    , dragState : Maybe ( NodePath, ( Float, Float ) )
    }



-- Model


type Model item
    = Model
        { tree : Tree.Tree item
        , new : Maybe NodePath
        , active : Maybe NodePath
        , focus : Maybe NodePath
        , drag : MultiDrag.Drag NodePath
        , displayRoot : Maybe NodePath
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


active : Model item -> Maybe item
active (Model { active, tree }) =
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
            | tree =
                model.new
                    |> Maybe.map
                        (\new ->
                            Tree.insert new item model.tree
                        )
                    |> Maybe.withDefault model.tree
        }


tree : Model item -> Tree.Tree item
tree (Model { tree }) =
    tree


isNew : Model item -> Bool
isNew (Model { new }) =
    new /= Nothing


getDropTarget : Config.Config item -> NodePath -> ( Float, Float ) -> Tree.Tree item -> Maybe NodePath
getDropTarget config path ( dragX, dragY ) tree =
    let
        ( x0, y0 ) =
            nodeGeometry config path tree
                |> Maybe.map .position
                |> Maybe.withDefault ( 0, 0 )

        ( x, y ) =
            ( x0 + dragX
            , y0 + dragY
            )
    in
        tree
            |> Tree.flatten
            |> List.filter
                (\( path_, _ ) ->
                    let
                        ( xo, yo ) =
                            nodeGeometry config path_ tree
                                |> Maybe.map .position
                                |> Maybe.withDefault ( 0, 0 )
                    in
                        path /= path_ && (abs (x - xo) < config.layout.width) && (abs (y - yo) < config.layout.height)
                )
            |> List.head
            |> Maybe.map Tuple.first



-- Msg


type Msg item
    = Activate NodePath
    | Deactivate
    | SetActive item
    | DeleteActive
    | SetNew NodePath
    | SetFocus (Maybe NodePath)
    | MouseMove Float Float
    | MouseDown NodePath Float Float
    | MouseUp Float Float



-- Update


update : Config.Config item -> Msg item -> Model item -> Model item
update config msg (Model model) =
    case msg of
        Activate path ->
            Model
                { model
                    | active =
                        if model.active == Just path then
                            Nothing
                        else
                            Just path
                    , new = Nothing
                }

        SetActive newItem ->
            Model
                { model
                    | tree =
                        model.active
                            |> Maybe.map (\active -> Tree.update active newItem model.tree)
                            |> Maybe.withDefault model.tree
                }

        DeleteActive ->
            Model
                { model
                    | tree =
                        model.active
                            |> Maybe.map (\active -> Tree.delete active model.tree)
                            |> Maybe.withDefault model.tree
                }

        SetFocus focus ->
            Model model

        SetNew nodePath ->
            Model
                { model | new = Just nodePath }

        Deactivate ->
            Model { model | active = Nothing, new = Nothing }

        MouseDown path x y ->
            Model
                { model
                    | drag =
                        MultiDrag.start path x y
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
                        |> Maybe.andThen
                            (\( path, dragOffset ) ->
                                getDropTarget config path dragOffset model.tree
                                    |> Maybe.map (\dropTargetPath -> Tree.swap path dropTargetPath model.tree)
                            )
                        |> Maybe.withDefault model.tree
            in
                Model
                    { model
                        | drag =
                            MultiDrag.init

                        -- , active = Nothing
                        , tree = newTree
                    }


type alias NodeGeometry =
    { position : ( Float, Float )
    , childSpan : Float
    , children : Int
    }


nodeGeometry : Config.Config item -> List Int -> Tree.Tree item -> Maybe NodeGeometry
nodeGeometry config path tree =
    tree
        |> Tree.analyze
        |> Tree.layout
        |> Dict.get path
        |> Maybe.map
            (\{ center, span, children } ->
                let
                    ( centerX, centerY ) =
                        center
                in
                    { position = ( centerX * (config.layout.width + config.layout.horizontalGap) + config.layout.width / 2 - config.layout.horizontalGap / 2, centerY * (config.layout.height + config.layout.verticalGap) )
                    , childSpan = span * (config.layout.width + config.layout.horizontalGap)
                    , children = children
                    }
            )


startsWith : List a -> List a -> Bool
startsWith start list =
    List.take (List.length start) list == start


view : Config.Config item -> List (Attribute (Msg item)) -> Model item -> Html (Msg item)
view config attrs (Model model) =
    let
        tree =
            Tree.addTrailingEmpties model.tree

        flatTree =
            Tree.flatten tree

        nodeBaseStyle =
            Styles.nodeBase config.layout.width config.layout.height

        coordStyle =
            Styles.coordinate config.layout.width
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
            (List.map
                (\( path, item ) ->
                    nodeGeometry config path tree
                        |> Maybe.map
                            (\{ position, childSpan, children } ->
                                let
                                    ( x, y ) =
                                        position

                                    dragState =
                                        MultiDrag.state model.drag

                                    ( isDragged, ( xDrag, yDrag ), draggedPath ) =
                                        dragState
                                            |> Maybe.map
                                                (\( draggedPath, offset ) ->
                                                    if startsWith draggedPath path then
                                                        ( True, offset, Just draggedPath )
                                                    else
                                                        ( False, ( 0, 0 ), Just draggedPath )
                                                )
                                            |> Maybe.withDefault ( False, ( 0, 0 ), Nothing )

                                    isDropTarget =
                                        dragState
                                            |> Maybe.map
                                                (\( draggedPath, offset ) ->
                                                    getDropTarget config draggedPath offset tree
                                                        |> Maybe.map (\dropTargetPath -> startsWith dropTargetPath path)
                                                        |> Maybe.withDefault False
                                                )
                                            |> Maybe.withDefault False

                                    isActive =
                                        model.active == Just path || isDragged

                                    xWithDrag =
                                        x + xDrag

                                    yWithDrag =
                                        y + yDrag
                                in
                                    [ p
                                        [ style <|
                                            nodeBaseStyle
                                                ++ (if item == Nothing then
                                                        Styles.placeholderNode
                                                    else
                                                        Styles.regularNode
                                                   )
                                                ++ (coordStyle xWithDrag yWithDrag)
                                                ++ [ ( "background-color"
                                                     , if isDropTarget then
                                                        red
                                                       else if (isActive && item /= Nothing) then
                                                        blue
                                                       else if item == Nothing then
                                                        "transparent"
                                                       else
                                                        gray
                                                     )
                                                   ]
                                                ++ [ ( "transition"
                                                     , if isDropTarget then
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
                                        , Utils.onClickStopPropagation
                                            (if item == Nothing then
                                                SetNew (List.take (List.length path - 1) path)
                                             else
                                                Activate path
                                            )
                                        , on "mousedown"
                                            (Decode.map2 (MouseDown path)
                                                (Decode.field "screenX" Decode.float)
                                                (Decode.field "screenY" Decode.float)
                                            )
                                        , on "mouseup"
                                            (Decode.map2 MouseUp
                                                (Decode.field "screenX" Decode.float)
                                                (Decode.field "screenY" Decode.float)
                                            )
                                        ]
                                        [ text
                                            (item
                                                |> Maybe.map config.view
                                                |> Maybe.withDefault "empty"
                                            )
                                        ]
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
                                        ++ [ button
                                                [ style <|
                                                    [ ( "position", "absolute" )
                                                    , ( "font-size", "8px" )
                                                    , ( "transform", "translate3d(-50%, -100%, 0)" )
                                                    , ( "z-index", "100" )
                                                    ]
                                                        ++ coordStyle (xWithDrag + config.layout.width / 2) (yWithDrag - 5)
                                                ]
                                                [ text "🔎"
                                                ]
                                           , svg
                                                [ width (toString (childSpan * 2))
                                                , height (toString config.layout.verticalGap)
                                                , viewBox <| "0 0 " ++ (toString (childSpan * 2)) ++ " " ++ (toString config.layout.verticalGap)
                                                , style <|
                                                    [ ( "position", "absolute" ) ]
                                                        ++ (coordStyle (xWithDrag - childSpan / 2 + config.layout.width / 2) (yWithDrag + config.layout.height))
                                                ]
                                                (Views.NodeConnectors.view children
                                                    childSpan
                                                    (config.layout.verticalGap)
                                                )
                                           ]
                            )
                        |> Maybe.withDefault []
                )
                flatTree
                |> List.foldl (++) []
            )
