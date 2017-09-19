module Geometry exposing (..)

import Dict
import Data.Tree as Tree
import Treditor.Config exposing (Config)
import Utils


type alias NodeId =
    String


type alias NodeGeometry =
    { position : ( Float, Float )
    , newChildPlaceholderPosition : ( Float, Float )
    , childSpan : Float
    }


type alias LocalGeometryContext =
    { depth : Int
    , index : Int
    }


nodeGeometry :
    Config item
    -> NodeId
    -> Tree.Tree item
    -> Maybe NodeGeometry
nodeGeometry config id tree =
    tree
        |> Tree.analyze config.toId
        |> Tree.layout 3
        |> Dict.get id
        |> Maybe.map
            (\{ center, span } ->
                let
                    ( centerX, centerY ) =
                        center
                in
                    { position = ( centerX * (config.layout.width + 20), centerY * (config.layout.height + config.layout.verticalGap) )
                    , childSpan = span * (config.layout.width + 20)
                    , newChildPlaceholderPosition = ( 0, 0 )
                    }
            )
