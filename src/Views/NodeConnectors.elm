module Views.NodeConnectors exposing (..)

import Html exposing (Html)
import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, x2, y1, y2, stroke, strokeWidth)


view : Float -> Float -> List (Html msg)
view w h =
    let
        pad =
            1
    in
        [ line
            [ x1 (w / 2 |> toString)
            , y1 (toString pad)
            , x2 (w / 2 |> toString)
            , y2 (h / 2 |> toString)
            , stroke "#CCCCCC"
            , strokeWidth "1"
            ]
            []
        , line
            [ x1 (toString pad)
            , y1 (h / 2 |> toString)
            , x2 (toString (w - pad))
            , y2 (h / 2 |> toString)
            , stroke "#CCCCCC"
            , strokeWidth "1"
            ]
            []
        , line
            [ x1 (toString pad)
            , y1 (h / 2 |> toString)
            , x2 (toString pad)
            , y2 (toString (h - pad))
            , stroke "#CCCCCC"
            , strokeWidth "1"
            ]
            []
        , line
            [ x1 (toString (w - pad))
            , y1 (h / 2 |> toString)
            , x2 (toString (w - pad))
            , y2 (toString (h - pad))
            , stroke "#CCCCCC"
            , strokeWidth "1"
            ]
            []
        ]
