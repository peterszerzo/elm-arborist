module Views.NodeConnectors exposing (..)

import Html exposing (Html)
import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, stroke, strokeWidth)


strokeColor : String
strokeColor =
    "#AAAAAA"


view : Int -> Float -> Float -> List (Html msg)
view divs w h =
    let
        pad =
            1

        range =
            if (divs) > 1 then
                List.range 0 (divs - 1)
                    |> List.map (\val -> (toFloat val) / (toFloat divs - 1))
            else
                [ 0.5 ]
    in
        [ line
            [ x1 (w / 2 |> toString)
            , y1 (toString pad)
            , x2 (w / 2 |> toString)
            , y2 (h / 2 |> toString)
            , stroke strokeColor
            , strokeWidth "1"
            ]
            []
        ]
            ++ (if divs > 1 then
                    [ line
                        [ x1 (toString pad)
                        , y1 (h / 2 |> toString)
                        , x2 (toString (w - pad))
                        , y2 (h / 2 |> toString)
                        , stroke strokeColor
                        , strokeWidth "1"
                        ]
                        []
                    ]
                else
                    []
               )
            ++ (List.map
                    (\val ->
                        let
                            x =
                                val
                                    * w
                                    + (if val == 0 then
                                        pad
                                       else if val == 1 then
                                        -pad
                                       else
                                        0
                                      )
                        in
                            line
                                [ x1 (toString x)
                                , y1 (h / 2 |> toString)
                                , x2 (toString x)
                                , y2 (toString (h - pad))
                                , stroke strokeColor
                                , strokeWidth "1"
                                ]
                                []
                    )
                    range
               )
