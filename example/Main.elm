module Main exposing (..)

import Json.Decode as Decode
import Html exposing (Html, div, node, h1, p, text, beginnerProgram, label, input, map)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onInput)
import Tree
import Item
import Treditor
import Styles


type alias Model =
    { treditor : Treditor.Model Item.Item
    }


type Msg
    = TreditorMsg (Treditor.Msg Item.Item)


treditorConfig : Treditor.Config Item.Item
treditorConfig =
    { nodeId = (\item -> item.id)
    , nodeView = (\item -> item.value)
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TreditorMsg treditorMsg ->
            { model | treditor = Treditor.update treditorConfig treditorMsg model.treditor }


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text Styles.raw ]
        , div [ class "intro" ]
            [ h1 [] [ text "treeditor" ]
            , p [] [ text "Prototype for an interface that manipulates a tree structure. Swap nodes by dragging them on top of each other." ]
            ]
        , div [ style [ ( "text-align", "center" ) ] ]
            [ Treditor.view treditorConfig [ style Styles.box ] model.treditor |> Html.map TreditorMsg
            , div [ style <| Styles.box ++ [ ( "padding", "20px" ) ] ]
                [ Treditor.active treditorConfig model.treditor
                    |> Maybe.map
                        (\item ->
                            label [ style [ ( "margin-bottom", "40px" ) ] ]
                                [ text "Edit node", input [ value item.value, onInput (\newVal -> Treditor.setActive { item | value = newVal }) ] [] ]
                                |> Html.map TreditorMsg
                        )
                    |> Maybe.withDefault (p [] [ text "Click a node to edit.." ])
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
        { model = { treditor = Treditor.init example }
        , update = update
        , view = view
        }
