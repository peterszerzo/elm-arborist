port module Egg.Main exposing (..)

import Task
import Json.Decode as Decode
import Json.Encode as Encode
import Regex
import Html exposing (Html, header, div, node, h1, h2, h3, p, a, text, program, label, textarea, map, button)
import Html.Attributes exposing (class, style, value, type_, href, id)
import Html.Events exposing (onInput, onClick)
import Svg exposing (svg, path)
import Svg.Attributes exposing (viewBox, d, stroke)
import Arborist
import Arborist.Tree as Tree
import Arborist.Settings as Settings
import Arborist.Context exposing (NodeState(..))
import Egg.Styles as Styles
import Window exposing (size, resizes)


{-| The Node data type held in each of the tree's nodes.
-}
type alias Node =
    { code : String
    }


{-| Program model.
-}
type alias Model =
    { arborist : Arborist.Model Node

    -- Keep track of a to-be-inserted node
    , newNode : Node
    , windowSize : Window.Size
    }


{-| The starting tree.
-}
tree : Tree.Tree Node
tree =
    Tree.Node { code = """<div>
  {props.children}
</div>""" }
        [ Tree.Node { code = "<header>I'm Egg{props.children}</header>" }
            []
        , Tree.Node { code = "<main>World</main>" }
            [ Tree.Node { code = "<code>const a;</code>" } []
            ]
        , Tree.Node { code = "<footer>Made with a frying pan at ...</footer>" }
            []
        , Tree.Node { code = "<style>{`\nheader {\n  background-color: #FFF;\n}\n\nfooter {\n  background-color: #dedede;\n}\n`}</style>" } []
        ]


{-| Flatten
-}
flatten : Tree.Tree a -> List ( List Int, a )
flatten =
    flattenTail []


flattenTail : List Int -> Tree.Tree a -> List ( List Int, a )
flattenTail path tree =
    case tree of
        Tree.Empty ->
            []

        Tree.Node val children ->
            [ ( path, val ) ]
                ++ (List.indexedMap
                        (\index child ->
                            (flattenTail (path ++ [ index ]) child)
                        )
                        children
                        |> List.foldl (++) []
                   )


init : ( Model, Cmd Msg )
init =
    ( { arborist =
            Arborist.initWith
                [ Settings.centerOffset 0 -180
                , Settings.nodeHeight 30
                , Settings.level 80
                , Settings.nodeWidth 100
                , Settings.connectorStrokeAttributes
                    [ stroke "#454545"
                    ]
                ]
                tree
      , newNode = { code = "" }
      , windowSize = { width = 0, height = 0 }
      }
    , Task.perform Resize Window.size
    )


port code : Encode.Value -> Cmd msg


{-| Program message
-}
type Msg
    = ArboristMsg Arborist.Msg
    | EditNewNodeCode String
    | SetActive Node
    | DeleteActive
    | Resize Window.Size



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        flatTree =
            Arborist.tree model.arborist
                |> flatten

        sourceCode =
            flatTree
                |> List.map
                    (\( path, { code } ) ->
                        let
                            childrenCount =
                                flatTree
                                    |> List.filter (\( path_, _ ) -> List.take (List.length path) path_ == path && (List.length path_ == List.length path + 1))
                                    |> List.length

                            pathPrefix =
                                (String.join "" << List.map (\i -> toString (i + 1))) path

                            prefixedCode =
                                code
                                    |> Regex.replace (Regex.All) (Regex.regex "<Child") (\_ -> "<Child" ++ pathPrefix)
                                    |> Regex.replace (Regex.All)
                                        (Regex.regex "{props.children}")
                                        (\_ ->
                                            childrenCount
                                                |> List.range 1
                                                |> List.map toString
                                                |> List.map (\p -> "<Child" ++ pathPrefix ++ p ++ "/>")
                                                |> String.join ""
                                        )
                        in
                            "function Child" ++ pathPrefix ++ " (props) {\n return " ++ prefixedCode ++ "\n}"
                    )
                |> String.join "\n\n"
    in
        case msg of
            ArboristMsg arboristMsg ->
                ( { model | arborist = Arborist.update arboristMsg model.arborist }
                , code (Encode.object [ ( "sourcecode", Encode.string sourceCode ) ])
                )

            SetActive newNode ->
                ( { model | arborist = Arborist.setActiveNode newNode model.arborist }
                , Cmd.none
                )

            DeleteActive ->
                ( { model | arborist = Arborist.deleteActiveNode model.arborist }
                , Cmd.none
                )

            EditNewNodeCode val ->
                ( { model
                    | newNode = { code = val }
                  }
                , Cmd.none
                )

            Resize { width, height } ->
                ( { model
                    | arborist =
                        Arborist.resize (width // 2) height model.arborist
                    , windowSize =
                        { width = width
                        , height = height
                        }
                  }
                , Cmd.none
                )



-- View


viewHeader : Html Msg
viewHeader =
    header
        [ style
            [ ( "width", "100%" )
            , ( "height", "60px" )
            , ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "padding", "0 20px" )
            , ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "background-color", "#001122" )
            , ( "font-size", "1.5rem" )
            , ( "color", "#FFF" )
            , ( "border-bottom", "1px solid #454545" )
            ]
        ]
        [ text "Egg" ]


view : Model -> Html Msg
view model =
    div [] <|
        [ viewHeader
        , node "style" [] [ text Styles.raw ]
        ]
            ++ [ div
                    [ id "playground-container"
                    , style
                        [ ( "position", "absolute" )
                        , ( "background-color", "#FFF" )
                        , ( "top", "60px" )
                        , ( "left", "50%" )
                        , ( "width", "50%" )
                        , ( "height", "calc(100% - 60px)" )
                        , ( "z-index", "10000" )
                        , ( "border-left", "1px solid #dedede" )
                        ]
                    ]
                    []
               , -- For pop-up coordinates to work, include view in a container
                 div
                    [ style
                        [ ( "margin", "auto" )
                        , ( "position", "absolute" )
                        , ( "background-color", "#232323" )
                        , ( "top", "60px" )
                        , ( "left", "0" )
                        , ( "width", (toString (model.windowSize.width // 2)) ++ "px" )
                        , ( "height", (toString (model.windowSize.height - 60)) ++ "px" )
                        ]
                    ]
                 <|
                    [ Arborist.view nodeView [ style Styles.box ] model.arborist |> Html.map ArboristMsg ]
                        ++ (Arborist.activeNodeWithContext model.arborist
                                |> Maybe.map
                                    (\( item, { position } ) ->
                                        let
                                            ( x, y ) =
                                                position
                                        in
                                            [ div
                                                [ style <|
                                                    Styles.popup
                                                        ++ [ ( "left", (toString x) ++ "px" )
                                                           , ( "top", (toString y) ++ "px" )
                                                           , ( "height", "320px" )
                                                           , ( "width", "480px" )
                                                           ]
                                                ]
                                                (case item of
                                                    Just item ->
                                                        [ textarea [ style textareaStyle, value item.code, onInput (\val -> SetActive { item | code = val }) ] []
                                                        , button [ style Styles.button, onClick DeleteActive ] [ text "Delete" ]
                                                        ]

                                                    Nothing ->
                                                        [ textarea [ style textareaStyle, value model.newNode.code, onInput EditNewNodeCode ] []
                                                        , button [ style Styles.button, type_ "submit", onClick (SetActive model.newNode) ] [ text "Add node" ]
                                                        ]
                                                )
                                            ]
                                    )
                                |> Maybe.withDefault []
                           )
               ]


textareaStyle : List ( String, String )
textareaStyle =
    [ ( "font-family", "monospace" )
    , ( "min-height", "200px" )
    , ( "font-size", "1.25rem" )
    ]


firstOpeningTag : String -> String
firstOpeningTag code =
    code
        |> String.indices ">"
        |> List.head
        |> Maybe.map (\index -> String.left (index + 1) code)
        |> Maybe.withDefault "Tag"


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : Arborist.NodeView Node
nodeView context item =
    item
        |> Maybe.map
            (\item ->
                div
                    [ style <|
                        Styles.nodeContainer
                            ++ [ ( "background-color"
                                 , case context.state of
                                    Active ->
                                        Styles.green

                                    Hovered ->
                                        Styles.lightBlue

                                    DropTarget ->
                                        Styles.orange

                                    Normal ->
                                        Styles.blue
                                 )
                               , ( "color", "white" )
                               , ( "height", "30px" )
                               ]
                    ]
                    [ p [ style <| Styles.text ] [ text (firstOpeningTag item.code) ]
                    ]
            )
        |> Maybe.withDefault
            (div
                [ style <|
                    Styles.nodeContainer
                        ++ [ ( "height", "30px" ) ]
                        ++ (case context.state of
                                Active ->
                                    [ ( "background-color", Styles.green )
                                    , ( "color", "white" )
                                    , ( "border", "0" )
                                    ]

                                DropTarget ->
                                    [ ( "background-color", Styles.orange )
                                    , ( "border", "0" )
                                    , ( "color", "white" )
                                    ]

                                _ ->
                                    [ ( "background-color", "transparent" )
                                    , ( "border", "1px dashed #CECECE" )
                                    , ( "color", "white" )
                                    ]
                           )
                ]
                [ p [ style <| Styles.text ] [ text "+" ] ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Arborist.subscriptions model.arborist |> Sub.map ArboristMsg
        , Window.resizes Resize
        ]


{-| Entry point
-}
main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
