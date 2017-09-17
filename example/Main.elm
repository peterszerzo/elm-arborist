module Main exposing (..)

import Json.Decode as Decode
import Html exposing (Html, div, node, h1, h2, h3, p, text, beginnerProgram, label, input, map, button)
import Html.Attributes exposing (class, style, value, type_)
import Html.Events exposing (onInput, onClick)
import Data.BinaryTree as Tree
import Item
import Treditor
import Styles


type alias Model =
    { treditor : Treditor.Model Item.Item
    , newItem : Item.Item
    }


type Msg
    = TreditorMsg (Treditor.Msg Item.Item)
    | EditNewItemId String
    | EditNewItemValue String
    | AddNewItem
    | NoOp


treditorConfig : Treditor.Config Item.Item
treditorConfig =
    { toId = (\item -> item.id)
    , view = (\item -> item.value)
    , layout =
        { width = 120
        , height = 24
        , verticalGap = 60
        , horizontalGap = 20
        }
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TreditorMsg treditorMsg ->
            { model | treditor = Treditor.update treditorConfig treditorMsg model.treditor }

        EditNewItemId newId ->
            { model
                | newItem = Item.setId newId model.newItem
            }

        EditNewItemValue newValue ->
            { model
                | newItem = Item.setValue newValue model.newItem
            }

        AddNewItem ->
            { model | treditor = Treditor.setNew treditorConfig model.newItem model.treditor }

        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text Styles.raw ]
        , div [ class "intro" ]
            [ h1 [] [ text "treditor" ]
            , p [] [ text "Prototype for an interface that manipulates a tree structure. Swap nodes by dragging them on top of each other." ]
            ]
        , div [ style [ ( "text-align", "center" ) ] ]
            [ Treditor.view treditorConfig [ style Styles.box ] model.treditor |> Html.map TreditorMsg
            , div [ style <| Styles.box ++ [ ( "padding", "20px" ) ] ]
                [ Treditor.active treditorConfig model.treditor
                    |> Maybe.map
                        (\item ->
                            label []
                                [ text "Edit node", input [ value item.value, onInput (\newVal -> Treditor.setActive { item | value = newVal }) ] [] ]
                                |> Html.map TreditorMsg
                        )
                    |> Maybe.withDefault (p [] [ text "Click a node to edit.." ])
                , if Treditor.isNew model.treditor then
                    div []
                        [ h3 [] [ text "New node" ]
                        , label []
                            [ text "Unique id", input [ value model.newItem.id, onInput EditNewItemId ] [] ]
                        , label []
                            [ text "Value", input [ value model.newItem.value, onInput EditNewItemValue ] [] ]
                        , button [ type_ "submit", onClick AddNewItem ] [ text "Add node" ]
                        ]
                  else
                    (text "Click on an empty leaf to create a new node.")
                ]
            ]
        ]


example : Tree.Tree Item.Item
example =
    """
  {
    "value": {
      "id": "app",
      "value": "Apples"
    },
    "left": {
      "value": {
        "id": "p",
        "value": "Pears"
      },
      "left": null,
      "right": {
        "value": {
          "id": "oj",
          "value": "Oranges"
        },
        "left": null,
        "right": null
      }
    },
    "right": {
      "value": {
        "id": "pch",
        "value": "Peaches"
      },
      "left": null,
      "right": {
        "value": {
          "id": "pch2",
          "value": "Peaches2"
        },
        "left": null,
        "right": null
      }
    }
  }
  """
        |> Decode.decodeString (Tree.decoder Item.decoder)
        |> Result.toMaybe
        |> Maybe.withDefault Tree.Empty


main : Program Never Model Msg
main =
    beginnerProgram
        { model =
            { treditor = Treditor.init example
            , newItem = Item.init
            }
        , update = update
        , view = view
        }
