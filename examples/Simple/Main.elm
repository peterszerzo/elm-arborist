module Simple.Main exposing (..)

{-| A simple Arborist app modeling a conversation flow.
-}

import Html exposing (Html, div, node, h1, h2, h3, p, a, text, program, label, input, map, button)
import Html.Attributes exposing (class, style, value, type_, href)
import Html.Events exposing (onInput, onClick)
import Arborist
import Arborist.Settings as Settings
import Arborist.Tree as Tree
import Simple.Styles as Styles
import Time


{-| The Node data type held in each of the tree's nodes.
-}
type alias Node =
    { question : String
    , answer : String
    }


setQuestion : String -> Node -> Node
setQuestion val item =
    { item | question = val }


setAnswer : String -> Node -> Node
setAnswer val item =
    { item | answer = val }


{-| Program model.
-}
type alias Model =
    { arborist : Arborist.Model Node

    -- Keep track of a to-be-inserted node
    , newNode : Node
    }


{-| The starting tree.
-}
tree : Tree.Tree Node
tree =
    Tree.Node { answer = "", question = "Do you like trees?" }
        [ Tree.Node { answer = "yes", question = "How much?" }
            [ Tree.Node { answer = "A lot!", question = "Where were you all my life?" } []
            ]
        , Tree.Node { answer = "No.", question = "Seriously?" }
            [ Tree.Node { answer = "Yes", question = "How about rollercoasters?" } []
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { arborist =
            Arborist.initWith
                [ Settings.centerOffset 0 -150
                , Settings.nodeHeight 45
                , Settings.level 100
                , Settings.nodeWidth 160
                , Settings.throttleMouseMoves (100 * Time.millisecond)

                --, Settings.sturdyMode True
                ]
                tree
      , newNode = { question = "", answer = "" }
      }
    , Cmd.none
    )


{-| Program message
-}
type Msg
    = ArboristMsg Arborist.Msg
    | EditNewNodeQuestion String
    | EditNewNodeAnswer String
    | SetActive Node
    | DeleteActive



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArboristMsg arboristMsg ->
            ( { model | arborist = Arborist.update arboristMsg model.arborist }
            , Cmd.none
            )

        SetActive newNode ->
            ( { model | arborist = Arborist.setActiveNode newNode model.arborist }
            , Cmd.none
            )

        DeleteActive ->
            ( { model | arborist = Arborist.deleteActiveNode model.arborist }
            , Cmd.none
            )

        EditNewNodeQuestion val ->
            ( { model
                | newNode = setQuestion val model.newNode
              }
            , Cmd.none
            )

        EditNewNodeAnswer val ->
            ( { model
                | newNode = setAnswer val model.newNode
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div [] <|
        [ node "style" [] [ text Styles.raw ]
        ]
            ++ [ -- For pop-up coordinates to work, include view in a container
                 div
                    [ style
                        [ ( "margin", "auto" )
                        , ( "position", "absolute" )
                        , ( "top", "0px" )
                        , ( "left", "0px" )
                        ]
                    ]
                 <|
                    [ Arborist.view nodeView [ style Styles.box ] model.arborist |> Html.map ArboristMsg
                    ]
                        ++ (Arborist.activeNode model.arborist
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
                                                           ]
                                                ]
                                                (case item of
                                                    Just item ->
                                                        [ label []
                                                            [ text "Question"
                                                            , input [ value item.question, onInput (\val -> SetActive { item | question = val }) ] []
                                                            ]
                                                        , label []
                                                            [ text "Answer"
                                                            , input [ value item.answer, onInput (\val -> SetActive { item | answer = val }) ] []
                                                            ]
                                                        , button [ style Styles.button, onClick DeleteActive ] [ text "Delete" ]
                                                        ]

                                                    Nothing ->
                                                        [ label []
                                                            [ text "Question", input [ value model.newNode.question, onInput EditNewNodeQuestion ] [] ]
                                                        , label []
                                                            [ text "Answer", input [ value model.newNode.answer, onInput EditNewNodeAnswer ] [] ]
                                                        , button [ style Styles.button, type_ "submit", onClick (SetActive model.newNode) ] [ text "Add node" ]
                                                        ]
                                                )
                                            ]
                                    )
                                |> Maybe.withDefault []
                           )
               ]


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
                                    Arborist.Active ->
                                        Styles.green

                                    Arborist.Hovered ->
                                        Styles.lightBlue

                                    Arborist.DropTarget ->
                                        Styles.orange

                                    Arborist.Normal ->
                                        Styles.blue
                                 )
                               , ( "color", "white" )
                               ]
                    ]
                    [ div [] <|
                        (if item.answer /= "" then
                            [ p
                                [ style <|
                                    bubble
                                        ++ []
                                ]
                                [ text item.answer ]
                            ]
                         else
                            []
                        )
                            ++ [ p [ style <| Styles.text ] [ text item.question ]
                               ]
                    ]
            )
        |> Maybe.withDefault
            (div
                [ style <|
                    Styles.nodeContainer
                        ++ (case context.state of
                                Arborist.Active ->
                                    [ ( "background-color", Styles.green )
                                    , ( "color", "white" )
                                    , ( "border", "0" )
                                    ]

                                Arborist.DropTarget ->
                                    [ ( "background-color", Styles.orange )
                                    , ( "border", "0" )
                                    , ( "color", "white" )
                                    ]

                                _ ->
                                    [ ( "background-color", "transparent" )
                                    , ( "border", "1px dashed #CECECE" )
                                    , ( "color", "#898989" )
                                    ]
                           )
                ]
                [ p [ style <| Styles.text ] [ text "New child" ] ]
            )


bubble : List ( String, String )
bubble =
    [ ( "position", "absolute" )
    , ( "box-sizing", "border-box" )
    , ( "width", "fit-content" )
    , ( "min-width", "48px" )
    , ( "height", "28px" )
    , ( "border-radius", "14px" )
    , ( "border", "2px solid #E2E2E2" )
    , ( "background-color", "#FFF" )
    , ( "font-size", "0.75rem" )
    , ( "padding", "0 12px" )
    , ( "color", "black" )
    , ( "display", "flex" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    , ( "top", "-46px" )
    , ( "left", "50%" )
    , ( "transform", "translateX(-50%)" )
    , ( "z-index", "200" )
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Arborist.subscriptions model.arborist |> Sub.map ArboristMsg


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
