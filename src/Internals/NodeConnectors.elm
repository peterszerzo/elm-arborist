module Internals.NodeConnectors exposing (pad, strokeWeight, toCoord, view)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Internals.Pt as Pt
import Internals.Settings as Settings
import Internals.Styles as Styles
import Svg exposing (line, svg)
import Svg.Attributes
    exposing
        ( height
        , stroke
        , strokeLinecap
        , strokeLinejoin
        , strokeWidth
        , viewBox
        , width
        , x1
        , x2
        , y1
        , y2
        )


pad : Float
pad =
    0


strokeWeight : Int
strokeWeight =
    2


toCoord : Float -> String
toCoord =
    floor >> String.fromInt


listMin : List Float -> Float
listMin =
    List.foldl min 100000


listMax : List Float -> Float
listMax =
    List.foldl max -100000


view :
    { settings : Settings.Settings node
    , extendTop : Bool
    , extendBottom : Bool
    , opacity : Float
    , offset : Pt.Pt
    , center : Pt.Pt
    , childCenters : List Pt.Pt
    }
    -> List (Html msg)
view config =
    let
        strokeAttrs =
            [ stroke config.settings.connectorStroke
            , strokeWidth config.settings.connectorStrokeWidth
            , strokeLinecap "round"
            , strokeLinejoin "round"
            ]

        ( centerX, centerY ) =
            config.center

        pts =
            config.center :: config.childCenters

        minX =
            pts
                |> List.map Tuple.first
                |> listMin

        minY =
            pts
                |> List.map Tuple.second
                |> listMin

        maxX =
            pts
                |> List.map Tuple.first
                |> listMax

        maxY =
            pts
                |> List.map Tuple.second
                |> listMax

        w_ =
            maxX - minX

        w =
            if w_ < 8 then
                8

            else
                w_

        h_ =
            maxY - minY - config.settings.nodeHeight

        h =
            if h_ < 0 then
                config.settings.level

            else
                h_

        relCenter =
            ( Tuple.first config.center - minX, Tuple.second config.center - minY )

        relChildren =
            List.map
                (\( x, y ) ->
                    ( x - minX
                    , y - minY
                    )
                )
                config.childCenters

        extender : { isTop : Bool } -> Html.Html msg
        extender extenderConfig =
            svg
                ([ width "4"
                 , height (String.fromFloat config.settings.extendConnectorsBy)
                 , viewBox <| "-2 0 4 " ++ String.fromFloat config.settings.extendConnectorsBy
                 , style "position" "absolute"
                 , style "z-index" "-1"
                 , style "opacity" <| String.fromFloat config.opacity
                 ]
                    ++ (Styles.coordinate config.settings
                            (( centerX + (config.settings.nodeWidth / 2)
                             , centerY
                                + (if extenderConfig.isTop then
                                    -config.settings.extendConnectorsBy

                                   else
                                    config.settings.nodeHeight
                                  )
                             )
                                |> Pt.add config.offset
                            )
                            |> List.map (\( property, value ) -> style property value)
                       )
                )
                [ line
                    ([ x1 "0"
                     , y1 "0"
                     , x2 "0"
                     , y2 (String.fromFloat config.settings.extendConnectorsBy)
                     ]
                        ++ strokeAttrs
                    )
                    []
                ]
    in
    [ if config.extendTop then
        extender { isTop = True }

      else
        text ""
    , if config.extendBottom then
        extender { isTop = False }

      else
        text ""
    , svg
        ([ width (String.fromFloat (w + 4))
         , height (String.fromFloat h)
         , viewBox <|
            if List.length config.childCenters == 0 then
                "-4 0 4 " ++ String.fromFloat h

            else
                "-2 0 " ++ String.fromFloat (w + 4) ++ " " ++ String.fromFloat h
         , style "position" "absolute"
         , style "z-index" "-1"
         , style "opacity" <| String.fromFloat config.opacity
         ]
            ++ (Styles.coordinate config.settings
                    (( minX + (config.settings.nodeWidth / 2)
                     , minY + config.settings.nodeHeight
                     )
                        |> Pt.add config.offset
                    )
                    |> List.map (\( property, value ) -> style property value)
               )
        )
      <|
        if List.length config.childCenters == 0 then
            []

        else
            line
                ([ x1 <| toCoord (Tuple.first relCenter)
                 , y1 <| toCoord pad
                 , x2 <| toCoord (Tuple.first relCenter)
                 , y2 <| toCoord (h / 2)
                 ]
                    ++ strokeAttrs
                )
                []
                :: List.map
                    (\( x, _ ) ->
                        line
                            ([ x1 <| toCoord x
                             , y1 <| toCoord (h / 2)
                             , x2 <| toCoord x
                             , y2 <| toCoord (h - pad)
                             ]
                                ++ strokeAttrs
                            )
                            []
                    )
                    relChildren
                ++ (if List.length config.childCenters > 1 then
                        [ line
                            ([ x1 <| toCoord 1
                             , y1 <| toCoord (h / 2)
                             , x2 <| toCoord (w - 1)
                             , y2 <| toCoord (h / 2)
                             ]
                                ++ strokeAttrs
                            )
                            []
                        ]

                    else
                        []
                   )
    ]
