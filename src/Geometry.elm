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


type alias LocalGeometryContext =
    { depth : Int
    , index : Int
    }


childNodeHorizontalOffset : Float -> Int -> Float
childNodeHorizontalOffset width depth =
    if depth == 0 then
        width + 2 * 10
    else
        width / 2 + 10


nodeGeometryTail :
    Config item
    -> LocalGeometryContext
    -> NodeId
    -> Tree.Tree item
    -> Maybe NodeGeometry
nodeGeometryTail config localGeoContext id tree =
    let
        childOffset =
            childNodeHorizontalOffset config.layout.width localGeoContext.depth
    in
        case tree of
            Tree.Empty ->
                Nothing

            Tree.Node item children ->
                let
                    childrenLength =
                        List.length children + 1

                    expandFactor =
                        if List.length children > 2 then
                            (childrenLength - 1 |> toFloat)
                        else
                            1
                in
                    if config.toId item == id then
                        Just
                            { position = ( 0, 0 )
                            , childOffset = childOffset * expandFactor
                            }
                    else
                        List.indexedMap
                            (\index tree ->
                                let
                                    normalizedCoordinate =
                                        if childrenLength > 1 then
                                            (toFloat index - (toFloat (childrenLength - 1)) / 2)
                                                / (toFloat (childrenLength - 1))
                                                * 2
                                        else
                                            0

                                    offset =
                                        ( childOffset * normalizedCoordinate * expandFactor
                                        , (config.layout.height + config.layout.verticalGap)
                                        )
                                in
                                    nodeGeometryTail config
                                        ({ depth = localGeoContext.depth + 1
                                         , index = index
                                         }
                                        )
                                        id
                                        tree
                                        |> Maybe.map
                                            (\{ position, childOffset } ->
                                                { position = Utils.addFloatTuples offset position
                                                , childOffset = childOffset * expandFactor
                                                }
                                            )
                            )
                            children
                            |> List.filterMap identity
                            |> List.head


nodeGeometry : Config item -> NodeId -> Tree.Tree item -> Maybe NodeGeometry
nodeGeometry config =
    nodeGeometryTail config
        { depth = 0
        , index = 0
        }
