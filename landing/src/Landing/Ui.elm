module Landing.Ui exposing (black, blue, blueRgb, bodyType, button, faintBlue, input, largeShadow, lighterBlue, red, rgbToColor, rgbToCssString, smallShadow, smallType, switch, white)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


blueRgb : ( Int, Int, Int )
blueRgb =
    ( 50, 57, 91 )


blue : Color
blue =
    rgbToColor blueRgb


lighterBlue : Color
lighterBlue =
    rgb255 68 75 105


red : Color
red =
    rgb255 220 40 30


faintBlue : Color
faintBlue =
    rgb255 199 201 210


rgbToColor : ( Int, Int, Int ) -> Color
rgbToColor ( r, g, b ) =
    rgb255 r g b


rgbToCssString : ( Int, Int, Int ) -> String
rgbToCssString ( r, g, b ) =
    "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"


smallShadow : Attribute msg
smallShadow =
    Border.shadow
        { offset = ( 0, 2 )
        , size = 0
        , blur = 12
        , color = rgba255 0 0 0 0.1
        }


largeShadow : Attribute msg
largeShadow =
    Border.shadow
        { offset = ( 0, 2 )
        , size = 0
        , blur = 20
        , color = rgba255 0 0 0 0.25
        }


bodyType : List (Attribute msg)
bodyType =
    [ Font.size 14
    , Font.family
        [ Font.typeface "Source Sans Pro"
        ]
    ]


smallType : List (Attribute msg)
smallType =
    [ Font.size 11
    , Font.family
        [ Font.typeface "Source Sans Pro"
        ]
    ]


switch :
    List (Attribute msg)
    ->
        { label : String
        , checked : Bool
        , onChange : Bool -> msg
        }
    -> Element msg
switch attrs config =
    Input.checkbox attrs
        { checked = config.checked
        , onChange = config.onChange
        , icon =
            \checked ->
                el
                    [ width (px 80)
                    , Font.center
                    ]
                <|
                    el
                        ([ width (px 40)
                         , height (px 18)
                         , padding 3
                         , Border.width 1
                         , Border.rounded 18
                         , Border.color blue
                         , centerX
                         ]
                            ++ (if checked then
                                    [ Background.color blue
                                    ]

                                else
                                    []
                               )
                        )
                    <|
                        el
                            ([ width (px 12)
                             , height (px 12)
                             , Font.center
                             , moveUp 1
                             , moveRight
                                (if checked then
                                    20

                                 else
                                    0
                                )
                             , centerY
                             , Border.rounded 14
                             ]
                                ++ (if checked then
                                        [ Background.color white
                                        ]

                                    else
                                        [ Background.color blue
                                        ]
                                   )
                            )
                            none
        , label =
            el
                (smallType
                    ++ [ Font.color black
                       , Font.center
                       ]
                )
                (text config.label)
                |> Input.labelAbove
                    [ centerX
                    ]
        }


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : String
        , isError : Bool
        }
    -> Element msg
button attrs config =
    Input.button
        (bodyType
            ++ [ Font.color white
               , paddingXY 10 8
               , Border.rounded 4
               ]
            ++ (if config.isError then
                    [ Background.color red
                    ]

                else
                    [ Background.color blue
                    , focused
                        []
                    ]
               )
            ++ attrs
        )
        { onPress = config.onPress
        , label = el bodyType (text config.label)
        }


input :
    { value : String
    , onChange : String -> msg
    , label : Maybe String
    }
    -> Element msg
input config =
    Input.text
        [ Font.size 16
        , paddingXY 10 6
        , Font.family
            [ Font.typeface "Source Sans Pro"
            ]
        , focused
            [ Border.shadow
                { size = 3
                , blur = 0
                , offset = ( 0, 0 )
                , color = faintBlue
                }
            , Border.color blue
            ]
        ]
        { onChange = config.onChange
        , text = config.value
        , placeholder = Nothing
        , label =
            config.label
                |> Maybe.map
                    (text
                        >> el
                            [ htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                            , Font.size 12
                            ]
                        >> Input.labelAbove
                            [ paddingEach
                                { top = 0
                                , bottom = 4
                                , left = 10
                                , right = 0
                                }
                            ]
                    )
                |> Maybe.withDefault (Input.labelHidden "Label")
        }
