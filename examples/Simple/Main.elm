module Simple.Main exposing (main)

{-| A simple Arborist app modeling a conversation flow.
-}

import Arborist
import Arborist.Settings as Settings
import Arborist.Tree as Tree
import Browser
import Html exposing (Html, a, button, div, h1, h2, h3, input, label, map, node, p, text)
import Html.Attributes exposing (class, href, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Simple.Styles as Styles


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


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    ( { arborist =
            Arborist.initWith
                [ Settings.centerOffset 0 -150
                , Settings.nodeHeight 45
                , Settings.level 100
                , Settings.nodeWidth 160
                , Settings.sturdyMode False
                , Settings.canvasWidth 1000
                , Settings.canvasHeight 600
                , Settings.defaultNode { question = "abc", answer = "def" }
                , Settings.showPlaceholderLeaves True
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
            ( { model | arborist = Arborist.setActiveNodeWithChildren newNode (Just []) model.arborist }
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
                    [ style "margin" "auto"
                    , style "position" "absolute"
                    , style "top" "0px"
                    , style "left" "0px"
                    ]
                 <|
                    [ Arborist.view nodeView
                        (Styles.box
                            |> List.map (\( property, value ) -> style property value)
                        )
                        model.arborist
                        |> Html.map ArboristMsg
                    ]
                        ++ (Arborist.activeNode model.arborist
                                |> Maybe.map
                                    (\( item, { position } ) ->
                                        let
                                            ( x, y ) =
                                                position
                                        in
                                        [ div
                                            (List.map (\( property, value ) -> style property value) Styles.popup
                                                ++ [ style "left" <| String.fromFloat x ++ "px"
                                                   , style "top" <| String.fromFloat y ++ "px"
                                                   ]
                                            )
                                            (case item of
                                                Just justItem ->
                                                    [ label []
                                                        [ text "Question"
                                                        , input
                                                            [ value justItem.question
                                                            , onInput (\val -> SetActive { justItem | question = val })
                                                            ]
                                                            []
                                                        ]
                                                    , label []
                                                        [ text "Answer"
                                                        , input
                                                            [ value justItem.answer
                                                            , onInput (\val -> SetActive { justItem | answer = val })
                                                            ]
                                                            []
                                                        ]
                                                    , button
                                                        ((Styles.button
                                                            |> List.map (\( property, value ) -> style property value)
                                                         )
                                                            ++ [ onClick DeleteActive
                                                               ]
                                                        )
                                                        [ text "Delete" ]
                                                    ]

                                                Nothing ->
                                                    [ label []
                                                        [ text "Question", input [ value model.newNode.question, onInput EditNewNodeQuestion ] [] ]
                                                    , label []
                                                        [ text "Answer", input [ value model.newNode.answer, onInput EditNewNodeAnswer ] [] ]
                                                    , button
                                                        ((Styles.button
                                                            |> List.map (\( property, value ) -> style property value)
                                                         )
                                                            ++ [ type_ "submit"
                                                               , onClick (SetActive model.newNode)
                                                               ]
                                                        )
                                                        [ text "Add node" ]
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
nodeView context maybeNode =
    maybeNode
        |> Maybe.map
            (\node ->
                div
                    (Styles.nodeContainer
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
                        |> List.map (\( property, value ) -> style property value)
                    )
                    [ div [] <|
                        (if node.answer /= "" then
                            [ Styles.bubble node.answer
                            ]

                         else
                            []
                        )
                            ++ [ Styles.bodyText node.question
                               ]
                    ]
            )
        |> Maybe.withDefault
            (div
                (Styles.nodeContainer
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
                    |> List.map (\( property, value ) -> style property value)
                )
                [ Styles.bodyText "New child"
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Arborist.subscriptions model.arborist |> Sub.map ArboristMsg


{-| Entry point
-}
main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
