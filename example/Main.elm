module Main exposing (..)

import Task
import Json.Decode as Decode
import Html exposing (Html, div, node, h1, h2, h3, p, a, text, program, label, input, map, button)
import Html.Attributes exposing (class, style, value, type_, href)
import Html.Events exposing (onInput, onClick)
import Arborist
import Arborist.Tree as Tree
import Arborist.Settings as Settings
import Arborist.Context exposing (NodeState(..))
import Styles
import Window exposing (size, resizes)


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
    , windowSize : Window.Size
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
    ( { arborist = Arborist.initWith [ Settings.centerOffset 0 -150 ] tree
      , newNode = { question = "", answer = "" }
      , windowSize = { width = 0, height = 0 }
      }
    , Task.perform Resize Window.size
    )


{-| Program message
-}
type Msg
    = ArboristMsg Arborist.Msg
    | EditNewNodeQuestion String
    | EditNewNodeAnswer String
    | SetActive Node
    | DeleteActive
    | Resize Window.Size



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

        Resize { width, height } ->
            ( { model | arborist = Arborist.resize width height model.arborist }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div [] <|
        [ node "style" [] [ text Styles.raw ]
        , div [ class "intro" ]
            [ h1 [] [ text "elm-arborist" ]
            , a [ href "https://github.com/peterszerzo/elm-arborist" ] [ text "GitHub" ]
            , p [ class "intro__icon" ] [ text "🌲" ]
            ]
        ]
            ++ [ -- For pop-up coordinates to work, include view in a container
                 div
                    [ style
                        [ ( "margin", "auto" )
                        , ( "position", "absolute" )
                        , ( "top", "0" )
                        , ( "left", "0" )
                        , ( "width", (toString model.windowSize.width) ++ "px" )
                        , ( "height", (toString model.windowSize.height) ++ "px" )
                        ]
                    ]
                 <|
                    [ Arborist.view nodeView [ style Styles.box ] model.arborist |> Html.map ArboristMsg ]
                        ++ (Arborist.activeNode2 model.arborist
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
                               ]
                    ]
                    [ div [] <|
                        (if item.answer /= "" then
                            [ p
                                [ style <|
                                    Styles.bubble
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
                                    , ( "color", "black" )
                                    ]
                           )
                ]
                [ p [ style <| Styles.text ] [ text "New child" ] ]
            )


{-| Entry point
-}
main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions =
            (\model ->
                Sub.batch
                    [ Arborist.subscriptions model.arborist |> Sub.map ArboristMsg
                    , Window.resizes Resize
                    ]
            )
        }
