module Landing.Ui exposing
    ( white, black, green, lighterGreen, faintGreen, greenRgb, red
    , bodyType, smallType, labelType, titleType, headingType, linkType
    , rgbToCssString, rgbToColor, htmlStyle
    , button, switch
    , largeShadow, smallShadow
    , input
    )

{-|

@docs white, black, green, lighterGreen, faintGreen, greenRgb, red

@docs bodyType, smallType, labelType, titleType, headingType, linkType

@docs rgbToCssString, rgbToColor, htmlStyle

@docs button, switch

@docs largeShadow, smallShadow

-}

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


greenRgb : ( Int, Int, Int )
greenRgb =
    ( 15, 73, 62 )


green : Color
green =
    rgbToColor greenRgb


lighterGreen : Color
lighterGreen =
    rgb255 56 96 88


faintGreen : Color
faintGreen =
    rgb255 210 219 217


red : Color
red =
    rgb255 220 40 30


faintRed : Color
faintRed =
    rgb255 255 222 221



-- Helpers


rgbToColor : ( Int, Int, Int ) -> Color
rgbToColor ( r, g, b ) =
    rgb255 r g b


rgbToCssString : ( Int, Int, Int ) -> String
rgbToCssString ( r, g, b ) =
    "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"


htmlStyle : String -> String -> Attribute msg
htmlStyle attrName attrValue =
    Html.Attributes.style attrName attrValue
        |> htmlAttribute


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
        , blur = 16
        , color = rgba255 0 0 0 0.35
        }


font : Attribute msg
font =
    Font.family
        [ Font.typeface "Monoid"
        , Font.typeface "monospace"
        ]


titleType : List (Attribute msg)
titleType =
    [ Font.size 24
    , font
    ]


headingType : List (Attribute msg)
headingType =
    [ Font.size 18
    , font
    ]


bodyType : List (Attribute msg)
bodyType =
    [ Font.size 12
    , font
    ]


linkType : List (Attribute msg)
linkType =
    [ Font.size 12
    , Font.underline
    , font
    ]


smallType : List (Attribute msg)
smallType =
    [ Font.size 9
    , font
    ]


labelType : List (Attribute msg)
labelType =
    [ Font.size 9
    , htmlStyle "text-transform" "uppercase"
    , alpha 0.75
    , Font.letterSpacing 0.5
    , font
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
                    , centerX
                    , Font.center
                    ]
                <|
                    el
                        ([ width (px 40)
                         , height (px 18)
                         , padding 3
                         , Border.width 1
                         , Border.rounded 18
                         , Border.color green
                         , centerX
                         , moveRight 10
                         ]
                            ++ (if checked then
                                    [ Background.color green
                                    , mouseOver
                                        [ Background.color lighterGreen
                                        ]
                                    ]

                                else
                                    [ mouseOver
                                        [ Background.color faintGreen
                                        ]
                                    ]
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
                                        [ Background.color green
                                        ]
                                   )
                            )
                            none
        , label =
            el
                (labelType
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
               , Font.center
               , paddingXY 10 11
               , Border.rounded 4
               ]
            ++ (if config.isError then
                    [ Border.width 1
                    , Border.color red
                    , Font.color red
                    , mouseOver
                        [ Background.color faintRed
                        ]
                    ]

                else
                    [ Background.color green
                    , mouseOver
                        [ Background.color lighterGreen
                        ]
                    , focused
                        [ Border.shadow
                            { offset = ( 0, 0 )
                            , size = 3
                            , blur = 0
                            , color = faintGreen
                            }
                        ]
                    ]
               )
            ++ attrs
        )
        { onPress = config.onPress
        , label = text config.label
        }


input :
    { value : String
    , onChange : String -> msg
    , label : Maybe String
    }
    -> Element msg
input config =
    Input.text
        (bodyType
            ++ [ paddingXY 10 8
               , Border.rounded 4
               , focused
                    [ Border.shadow
                        { size = 3
                        , blur = 0
                        , offset = ( 0, 0 )
                        , color = faintGreen
                        }
                    , Border.color green
                    ]
               ]
        )
        { onChange = config.onChange
        , text = config.value
        , placeholder = Nothing
        , label =
            config.label
                |> Maybe.map
                    (text
                        >> el
                            labelType
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
