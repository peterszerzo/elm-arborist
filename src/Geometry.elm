module Geometry exposing (..)

import Data.BinaryTree as Tree
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

            Tree.Node item left right ->
                if config.toId item == id then
                    Just
                        { position = ( 0, 0 )
                        , childOffset = childOffset
                        }
                else
                    (let
                        ( leftGeometry, leftOffsetPt ) =
                            ( nodeGeometryTail config (depth + 1) id left, ( -childOffset, (config.layout.height + config.layout.verticalGap) ) )

                        ( rightGeometry, rightOffsetPt ) =
                            ( nodeGeometryTail config (depth + 1) id right, ( childOffset, (config.layout.height + config.layout.verticalGap) ) )
                     in
                        case ( leftGeometry, rightGeometry ) of
                            ( Just leftGeometry, _ ) ->
                                Just
                                    { position = Utils.addFloatTuples leftOffsetPt leftGeometry.position
                                    , childOffset = leftGeometry.childOffset
                                    }

                            ( _, Just rightGeometry ) ->
                                Just
                                    { position = Utils.addFloatTuples rightOffsetPt rightGeometry.position
                                    , childOffset = rightGeometry.childOffset
                                    }

                            ( _, _ ) ->
                                Nothing
                    )


nodeGeometry : Config item -> NodeId -> Tree.Tree item -> Maybe NodeGeometry
nodeGeometry config =
    nodeGeometryTail config 0
