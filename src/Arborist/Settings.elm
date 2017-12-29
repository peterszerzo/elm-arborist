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

{-| Various settings for the editor, defined at the time of [initialization](Arborist#initWith), or [added](Arborist#applySettings) at any time later in the program. Includes various geometric settings such as canvas dimensions and the gutter between nodes, and, in a later version of this package, more functional settings such as hiding placeholder nodes.

@docs nodeWidth, nodeHeight, canvasWidth, canvasHeight, level, gutter, centerOffset

-}

import Data.Settings exposing (Setting(..))
import Svg


{-| Sets the width of node.
-}
nodeWidth : Int -> Setting
nodeWidth =
    NodeWidth


{-| Sets the width of node.
-}
nodeHeight : Int -> Setting
nodeHeight =
    NodeHeight


{-| Sets the width of the canvas.
-}
canvasWidth : Int -> Setting
canvasWidth =
    CanvasWidth


{-| Sets the height of the canvas.
-}
canvasHeight : Int -> Setting
canvasHeight =
    CanvasHeight


{-| Vertical distance between the bottom of a parent node and the top of its child.
-}
level : Int -> Setting
level =
    Level


{-| Horizontal gutter between nodes.
-}
gutter : Int -> Setting
gutter =
    Gutter


{-| When a node is activated, the interface centers it automatically. Sometimes, though, it is useful to move it somewhere other than the center, e.g. to accommodate a large pop-up underneath. With this configuration option, you can specify this offset, horizontal first, vertical second.
-}
centerOffset : Int -> Int -> Setting
centerOffset =
    CenterOffset
