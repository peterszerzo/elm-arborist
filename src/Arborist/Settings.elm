module Arborist.Settings
    exposing
        ( nodeWidth
        , nodeHeight
        , canvasWidth
        , canvasHeight
        , level
        , gutter
        , centerOffset
        , connectorStrokeAttributes
        , dragAndDrop
        , placeholderLeaves
        , throttleMouseMoves
        , sturdyMode
        )

{-| Various settings for the editor, defined at the time of [initialization](Arborist#initWith), or [added](Arborist#applySettings) at any time later in the program. Includes various geometric settings such as canvas dimensions and the gutter between nodes, and, in a later version of this package, more functional settings such as hiding placeholder nodes.

@docs nodeWidth, nodeHeight, canvasWidth, canvasHeight, level, gutter, centerOffset, connectorStrokeAttributes, dragAndDrop, placeholderLeaves, throttleMouseMoves, sturdyMode

-}

import Svg
import Internal.Settings exposing (Setting(..))
import Time


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


{-| Use this to specify SVG stroke attributes that will be applied for all paths linking nodes together, e.g:

    connectorStrokeAttributes [ strokeWidth "3", stroke "#EFEFEF", strokeLinecap "round" ]

Note the `Never` in the type signature: the compiler won't allow any event handlers with messages. Those are swallowed with an `Html.Attributes.map`.

-}
connectorStrokeAttributes : List (Svg.Attribute Never) -> Setting
connectorStrokeAttributes =
    ConnectorStrokeAttributes


{-| Enable or disable drag and drop with this boolean flag (`True` is enabled, which is the default value). Note that the tree can still be panned if drag and drop is disabled.
-}
dragAndDrop : Bool -> Setting
dragAndDrop =
    DragAndDrop


{-| Set whether placeholder leaves should be displayed.
-}
placeholderLeaves : Bool -> Setting
placeholderLeaves =
    PlaceholderLeaves


{-| Turn on mouse move throttling over a specified interval. You must connect `subscriptions` for this to work.
-}
throttleMouseMoves : Time.Time -> Setting
throttleMouseMoves =
    ThrottleMouseMoves


{-| Enables sturdy mode: as long as the tree is not empty, there is always one active node. Use this mode when it is critical that active node-related feedback doesn't leave the screen.
-}
sturdyMode : Bool -> Setting
sturdyMode =
    SturdyMode
