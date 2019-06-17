module Landing.Minimap exposing (arboristSettings, canvasHeight, canvasWidth, nodeView)

import Arborist
import Arborist.Settings as Settings
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Landing.Node as Node
import Landing.Ui as Ui


canvasWidth : Int
canvasWidth =
    240


canvasHeight : Int
canvasHeight =
    200


arboristSettings : List (Arborist.Setting Node.Node)
arboristSettings =
    [ Settings.keyboardNavigation False
    , Settings.connectorStroke "#000"
    , Settings.canvasWidth canvasWidth
    , Settings.canvasHeight canvasWidth
    , Settings.showPlaceholderLeaves True
    , Settings.nodeWidth 20
    , Settings.nodeHeight 20
    , Settings.level 25
    , Settings.gutter 20
    ]


pinContainer : List (Attribute msg) -> Element msg -> Element msg
pinContainer attrs child =
    el
        ([ width (px 20)
         , height (px 20)
         , Border.rounded 10
         , moveRight 2
         ]
            ++ attrs
        )
        child


nodeView : Arborist.NodeView Node.Node msg
nodeView _ maybeNode =
    (case maybeNode of
        Just node ->
            node.question
                |> String.left 1
                |> String.toUpper
                |> text
                |> el
                    [ centerX
                    , centerY
                    , Font.color Ui.white
                    , Font.size 10
                    ]
                |> pinContainer
                    [ Background.color Ui.blue
                    ]

        Nothing ->
            el
                [ centerX
                , centerY
                , Font.color Ui.blue
                , Font.size 16
                , Font.bold
                ]
                (text "+")
                |> pinContainer
                    [ Border.width 2
                    , Border.color Ui.blue
                    ]
    )
        |> layoutWith
            { options =
                [ noStaticStyleSheet
                ]
            }
            []
