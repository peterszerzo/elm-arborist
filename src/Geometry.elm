module Geometry exposing (..)

import Data.Tree as Tree
import Treditor.Config exposing (Config)
import Utils


type alias NodeId =
    String


type alias NodeGeometry =
    { position : ( Float, Float )
    , childOffset : Float
    }


childNodeHorizontalOffset : Float -> Int -> Float
childNodeHorizontalOffset width depth =
    if depth == 0 then
        width + 2 * 10
    else
        width / 2 + 10


nodeGeometryTail : Config item -> Int -> NodeId -> Tree.Tree item -> Maybe NodeGeometry
nodeGeometryTail config depth id tree =
    let
        childOffset =
            childNodeHorizontalOffset config.layout.width depth
    in
        case tree of
            Tree.Empty ->
                Nothing

            Tree.Node item children ->
                if config.toId item == id then
                    Just
                        { position = ( 0, 0 )
                        , childOffset = childOffset
                        }
                else
                    List.indexedMap
                        (\index tree ->
                            let
                                offset =
                                    ( childOffset * (toFloat index)
                                    , (config.layout.height + config.layout.verticalGap)
                                    )
                            in
                                nodeGeometryTail config (depth + 1) id tree
                                    |> Maybe.map
                                        (\{ position, childOffset } ->
                                            { position = Utils.addFloatTuples offset position, childOffset = childOffset }
                                        )
                        )
                        children
                        |> List.filterMap identity
                        |> List.head


nodeGeometry : Config item -> NodeId -> Tree.Tree item -> Maybe NodeGeometry
nodeGeometry config =
    nodeGeometryTail config 0
