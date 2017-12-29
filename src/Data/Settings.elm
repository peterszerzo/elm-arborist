module Data.Settings exposing (..)

import Svg


type alias Settings =
    { canvasWidth : Float
    , canvasHeight : Float
    , nodeWidth : Float
    , nodeHeight : Float
    , level : Float
    , gutter : Float
    , centerOffset : ( Float, Float )
    }


type Setting
    = NodeWidth Int
    | NodeHeight Int
    | CanvasWidth Int
    | CanvasHeight Int
    | Level Int
    | Gutter Int
    | CenterOffset Int Int


apply : List Setting -> Settings -> Settings
apply newSettings settings =
    case newSettings of
        [] ->
            settings

        head :: tail ->
            apply tail
                (case head of
                    NodeWidth w ->
                        { settings | nodeWidth = toFloat w }

                    NodeHeight h ->
                        { settings | nodeHeight = toFloat h }

                    CanvasWidth w ->
                        { settings | canvasWidth = toFloat w }

                    CanvasHeight h ->
                        { settings | canvasHeight = toFloat h }

                    Level l ->
                        { settings | level = toFloat l }

                    Gutter g ->
                        { settings | gutter = toFloat g }

                    CenterOffset x y ->
                        { settings | centerOffset = ( toFloat x, toFloat y ) }
                )
