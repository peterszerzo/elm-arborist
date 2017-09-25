module Main exposing (..)

import Json.Decode as Decode
import Html exposing (Html, div, node, h1, h2, h3, p, text, beginnerProgram, label, input, map, button)
import Html.Attributes exposing (class, style, value, type_)
import Html.Events exposing (onInput, onClick)
import Data.Tree as Tree
import Item
import Treditor
import Treditor.Config
import Styles


type alias NodeId =
    String


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


nodeContainerStyle : List ( String, String )
nodeContainerStyle =
    [ ( "width", "100%" )
    , ( "height", "40px" )
    , ( "overflow", "hidden" )
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


treditorConfig : Treditor.Config.Config Item.Item msg
treditorConfig =
    { view =
        (\context item ->
            div
                [ style <|
                    nodeContainerStyle
                        ++ [ ( "background-color"
                             , if context.isActive then
                                "#4DC433"
                               else if context.isDropTarget then
                                "#F18F01"
                               else
                                "#3E849B"
                             )
                           , ( "color", "white" )
                           ]
                ]
                [ p [ style <| textStyle ] [ text item.value ] ]
        )
    , placeholderView =
        (\context ->
            div
                [ style <|
                    nodeContainerStyle
                        ++ [ ( "background-color", "transparent" )
                           , ( "border", "1px dashed #CECECE" )
                           ]
                ]
                [ p [ style <| textStyle ] [ text "New child" ] ]
        )
    , layout =
        { width = 160
        , height = 40
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
            { model | treditor = Treditor.setNew model.newItem model.treditor }

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
            [ Treditor.view treditorConfig [ style [ ( "width", "100%" ), ( "height", "100%" ) ] ] model.treditor |> Html.map TreditorMsg ]
                ++ (Treditor.active model.treditor
                        |> Maybe.map
                            (\item ->
                                [ div [ style <| Styles.popup ]
                                    [ label []
                                        [ text "Edit node"
                                        , input [ value item.value, onInput (\newVal -> Treditor.SetActive { item | value = newVal }) ] []
                                        ]
                                    , button [ onClick Treditor.DeleteActive ] [ text "Delete" ]
                                    ]
                                    |> Html.map TreditorMsg
                                ]
                            )
                        |> Maybe.withDefault []
                   )
                ++ (if Treditor.isNew model.treditor then
                        [ div [ style Styles.popup ]
                            [ label []
                                [ text "Unique id", input [ value model.newItem.id, onInput EditNewItemId ] [] ]
                            , label []
                                [ text "Value", input [ value model.newItem.value, onInput EditNewItemValue ] [] ]
                            , button [ type_ "submit", onClick AddNewItem ] [ text "Add node" ]
                            ]
                        ]
                    else
                        []
                   )
        ]


example : Tree.Tree Item.Item
example =
    """
  {
    "value": {
      "id": "app",
      "value": "Apples"
    },
    "children": [
      {
        "value": {
          "id": "p",
          "value": "Pears"
        },
        "children": [
          {
            "value": {
              "id": "oj",
              "value": "Oranges"
            },
            "children": []
          },
          {
            "value": {
              "id": "oj2",
              "value": "Oranges"
            },
            "children": []
          }
        ]
      },
      {
        "value": {
          "id": "pch",
          "value": "Peaches"
        },
        "children": [
          {
            "value": {
              "id": "pch2",
              "value": "Peaches2"
            },
            "children": []
          },
          {
            "value": {
              "id": "pch3",
              "value": "Peaches3"
            },
            "children": []
          },
          {
            "value": {
              "id": "pch4",
              "value": "Peaches4"
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
            { treditor = Treditor.init example
            , newItem = Item.init
            }
        , update = update
        , view = view
        }
