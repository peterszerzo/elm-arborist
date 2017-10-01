module Main exposing (..)

import Json.Decode as Decode
import Html exposing (Html, div, node, h1, h2, h3, p, text, beginnerProgram, label, input, map, button)
import Html.Attributes exposing (class, style, value, type_)
import Html.Events exposing (onInput, onClick)
import Data.Tree as Tree
import Item
import Arborist
import Arborist.Config exposing (NodeState(..))
import Styles


type alias NodeId =
    String


type alias Model =
    { arborist : Arborist.Model Item.Item
    , newItem : Item.Item
    }


type Msg
    = ArboristMsg (Arborist.Msg Item.Item)
    | EditNewItemQuestion String
    | EditNewItemAnswer String
    | SetActive Item.Item
    | DeleteActive
    | NoOp


nodeContainerStyle : List ( String, String )
nodeContainerStyle =
    [ ( "width", "100%" )
    , ( "height", "60px" )
    , ( "border-radius", "4px" )
    , ( "padding", "4px 20px" )
    , ( "box-sizing", "border-box" )
    , ( "padding", "5px" )
    , ( "display", "flex" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    ]


textStyle : List ( String, String )
textStyle =
    [ ( "margin", "0" )
    , ( "line-height", "0.9" )
    , ( "text-align", "center" )
    ]


bubbleStyle : List ( String, String )
bubbleStyle =
    [ ( "position", "absolute" )
    , ( "box-sizing", "border-box" )
    , ( "border-radius", "6px" )
    , ( "border", "1px solid #3E849B" )
    , ( "width", "80px" )
    , ( "height", "30px" )
    , ( "padding-top", "6px" )
    , ( "color", "black" )
    , ( "text-align", "center" )
    , ( "background", "#FFFFFF" )
    , ( "top", "-58px" )
    , ( "left", "calc(50% - 40px + 2px)" )
    , ( "z-index", "200" )
    ]


arboristConfig : Arborist.Config.Config Item.Item msg
arboristConfig =
    { view =
        (\context item ->
            item
                |> Maybe.map
                    (\item ->
                        div
                            [ style <|
                                nodeContainerStyle
                                    ++ [ ( "background-color"
                                         , case context.state of
                                            Active ->
                                                "#4DC433"

                                            DropTarget ->
                                                "#F18F01"

                                            Normal ->
                                                "#3E849B"
                                         )
                                       , ( "color", "white" )
                                       ]
                            ]
                            [ div [] <|
                                (if item.answer /= "" then
                                    [ p
                                        [ style <|
                                            bubbleStyle
                                                ++ []
                                        ]
                                        [ text item.answer ]
                                    ]
                                 else
                                    []
                                )
                                    ++ [ p [ style <| textStyle ] [ text item.question ]
                                       ]
                            ]
                    )
                |> Maybe.withDefault
                    (div
                        [ style <|
                            nodeContainerStyle
                                ++ (case context.state of
                                        Active ->
                                            [ ( "background-color", "#4DC433" )
                                            , ( "color", "white" )
                                            , ( "border", "0" )
                                            ]

                                        DropTarget ->
                                            [ ( "background-color", "#F18F01" )
                                            , ( "border", "0" )
                                            , ( "color", "white" )
                                            ]

                                        Normal ->
                                            [ ( "background-color", "transparent" )
                                            , ( "border", "1px dashed #CECECE" )
                                            , ( "color", "black" )
                                            ]
                                   )
                        ]
                        [ p [ style <| textStyle ] [ text "New child" ] ]
                    )
        )
    , layout =
        { width = 200
        , height = 60
        , level = 120
        , gutter = 20
        }
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ArboristMsg arboristMsg ->
            { model | arborist = Arborist.update arboristConfig arboristMsg model.arborist }

        SetActive newItem ->
            { model | arborist = Arborist.setActive newItem model.arborist }

        DeleteActive ->
            { model | arborist = Arborist.deleteActive model.arborist }

        EditNewItemQuestion val ->
            { model
                | newItem = Item.setQuestion val model.newItem
            }

        EditNewItemAnswer val ->
            { model
                | newItem = Item.setAnswer val model.newItem
            }

        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text Styles.raw ]
        , div [ class "intro" ]
            [ h1 [] [ text "elm-arborist" ]
            , p [] [ text "a 🌲 editing interface" ]
            ]
        , div [ style Styles.box ] <|
            [ Arborist.view arboristConfig [ style [ ( "width", "100%" ), ( "height", "100%" ) ] ] model.arborist |> Html.map ArboristMsg ]
                ++ (Arborist.active arboristConfig model.arborist
                        |> Maybe.map
                            (\( item, ( x, y ) ) ->
                                [ div
                                    [ style <|
                                        Styles.popup
                                            ++ [ ( "left", (toString (x + 600 - 220)) ++ "px" )
                                               , ( "top", (toString (y + 130)) ++ "px" )
                                               ]
                                    ]
                                  <|
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
                                            , button [ onClick DeleteActive ] [ text "Delete" ]
                                            ]

                                        Nothing ->
                                            [ label []
                                                [ text "Question", input [ value model.newItem.question, onInput EditNewItemQuestion ] [] ]
                                            , label []
                                                [ text "Answer", input [ value model.newItem.answer, onInput EditNewItemAnswer ] [] ]
                                            , button [ type_ "submit", onClick (SetActive model.newItem) ] [ text "Add node" ]
                                            ]
                                    )
                                ]
                            )
                        |> Maybe.withDefault []
                   )
        ]


example : Tree.Tree Item.Item
example =
    """
  {
    "value": {
      "question": "Do you like trees?",
      "answer": ""
    },
    "children": [
      {
        "value": {
          "question": "How much?",
          "answer": "Yes"
        },
        "children": [
          {
            "value": {
              "question": "Where were you all my life?",
              "answer": "A lot!!"
            },
            "children": []
          }
        ]
      },
      {
        "value": {
          "question": "Seriously?",
          "answer": "No."
        },
        "children": [
          {
            "value": {
              "question": "How about rollercoasters?",
              "answer": "Yes."
            },
            "children": []
          }
        ]
      }
    ]
  }
  """
        |> Decode.decodeString (Tree.decoder Item.decoder)
        |> Result.toMaybe
        |> Maybe.withDefault Tree.Empty


main : Program Never Model Msg
main =
    beginnerProgram
        { model =
            { arborist = Arborist.init example
            , newItem = Item.init
            }
        , update = update
        , view = view
        }
