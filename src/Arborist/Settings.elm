module Arborist.Settings
    exposing
        ( nodeWidth
        , nodeHeight
        , canvasWidth
        , canvasHeight
        , level
        , gutter
        , centerOffset
        )

{-| Editor settings.

@docs nodeWidth, nodeHeight, canvasWidth, canvasHeight, level, gutter, centerOffset

-}

import Data.Settings exposing (Setting(..))


{-| Sets the width of a single node.
-}
nodeWidth : Int -> Setting
nodeWidth =
    NodeWidth


{-| Sets the width of a single node.
-}
nodeHeight : Int -> Setting
nodeHeight =
    NodeHeight


{-| Sets the width of a single canvas.
-}
canvasWidth : Int -> Setting
canvasWidth =
    CanvasWidth


{-| Sets the width of a single canvas.
-}
canvasHeight : Int -> Setting
canvasHeight =
    CanvasHeight


{-| Vertical height of a level, between the bottom of the upper node and the top of the lower one.
-}
level : Int -> Setting
level =
    Level


{-| Horizontal gutter between nodes.
-}
gutter : Int -> Setting
gutter =
    Gutter


{-| When a node is activated, the interface centers it automatically. Sometimes, though, it is useful to move it somewhere other than the center, e.g. to accommodate a large pop-up. With this configuration option, you can specify this dimension by passing a horizontal offset first and a vertical one second.
-}
centerOffset : Int -> Int -> Setting
centerOffset =
    CenterOffset
