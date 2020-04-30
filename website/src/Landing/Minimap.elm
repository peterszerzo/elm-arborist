module Landing.Minimap exposing
    ( canvasHeight
    , canvasWidth
    , gutter
    , level
    , minimapOnlyArboristSettings
    , nodeSize
    , nodeView
    )

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
    280


canvasHeight : Int
canvasHeight =
    200


nodeSize : Int
nodeSize =
    20


level : Int
level =
    20


gutter : Int
gutter =
    10


minimapOnlyArboristSettings : List (Arborist.Setting Node.Node)
minimapOnlyArboristSettings =
    [ Settings.keyboardNavigation False
    , Settings.connectorStroke <| Ui.rgbToCssString Ui.greenRgb
    , Settings.canvasWidth canvasWidth
    , Settings.canvasHeight canvasHeight
    , Settings.centerOffset 0 (-150 * 40 / 145 |> floor)
    , Settings.showPlaceholderLeaves True
    , Settings.nodeWidth nodeSize
    , Settings.nodeHeight nodeSize
    , Settings.level level
    , Settings.gutter gutter
    ]


pinContainer : List (Attribute msg) -> Element msg -> Element msg
pinContainer attrs child =
    el
        ([ width (px 20)
         , height (px 20)
         , Border.rounded 10
         , moveRight 2
         , pointer
         ]
            ++ attrs
        )
        child


nodeView : Arborist.NodeView Node.Node msg
nodeView nodeState maybeNode =
    (case maybeNode of
        Just node ->
            node.question
                |> String.left 1
                |> String.toUpper
                |> text
                |> el
                    (Ui.smallType
                        ++ [ centerX
                           , centerY
                           , Font.color Ui.white
                           ]
                    )
                |> pinContainer
                    ([ Background.color Ui.green
                     , mouseOver
                        [ Background.color Ui.lighterGreen
                        ]
                     ]
                        ++ (case nodeState.state of
                                Arborist.Active ->
                                    [ [ "0 0 0 1px white"
                                      , "0 0 0 3px " ++ Ui.rgbToCssString Ui.greenRgb
                                      ]
                                        |> String.join ","
                                        |> Ui.htmlStyle "box-shadow"
                                    ]

                                _ ->
                                    []
                           )
                    )

        Nothing ->
            el
                [ centerX
                , centerY
                , Font.color Ui.green
                , Font.size 16
                , Font.bold
                ]
                (text "+")
                |> pinContainer
                    [ Border.width 2
                    , Border.color Ui.green
                    , mouseOver
                        [ Background.color Ui.faintGreen
                        ]
                    ]
    )
        |> layoutWith
            { options =
                [ noStaticStyleSheet
                ]
            }
            []
