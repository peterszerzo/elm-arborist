module Main exposing (..)

import Json.Decode as Decode
import Html exposing (Html, div, node, h1, p, text, beginnerProgram, label, input, map)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onInput)
import Tree
import Item
import Treditor
import Colors exposing (..)


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


boxStyle : List ( String, String )
boxStyle =
    [ ( "width", "560px" )
    , ( "height", "560px" )
    , ( "margin", "10px" )
    , ( "text-align", "left" )
    , ( "display", "inline-block" )
    , ( "vertical-align", "top" )
    , ( "position", "relative" )
    , ( "border-radius", "4px" )
    , ( "border", "1px solid " ++ faintGray )
    ]


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text rawStyle ]
        , div [ class "intro" ]
            [ h1 [] [ text "treeditor" ]
            , p [] [ text "Prototype for an interface that manipulates a tree structure. Swap nodes by dragging them on top of each other." ]
            ]
        , div [ style [ ( "text-align", "center" ) ] ]
            [ Treditor.view treditorConfig [ style boxStyle ] model.treditor |> Html.map TreditorMsg
            , div [ style <| boxStyle ++ [ ( "padding", "20px" ) ] ]
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


rawStyle : String
rawStyle =
    """
* {
  -webkit-font-smoothing: antialiased;
  box-sizing: border-box;
  font-family: monospace;
}

.intro {
  text-align: center;
}

input {
  outline: none;
  display: block;
  width: 100%;
  padding: 4px 8px;
  margin-top: 4px;
  border-radius: 4px;
  border: 1px solid """ ++ faintGray ++ """;
  transition: border-color 0.3s;
}

h1 {
  font-size: 2rem;
  font-weight: 400;
}

h3 {
  font-size: 1.5rem;
  font-weight: 400;
}

h3:not(:first-child) {
  margin-top: 40px;
}

input:focus {
  border-color: """ ++ blue ++ """;
}

label {
  color: """ ++ gray ++ """;
}

input,
label,
p {
  font-size: 1rem;
}
"""


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
