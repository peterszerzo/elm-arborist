module Arborist.Settings exposing
    ( nodeWidth, nodeHeight, canvasWidth, canvasHeight, level, gutter, centerOffset, connectorStroke, connectorStrokeWidth
    , dragAndDrop, keyboardNavigation, keyboardNavigationOutside, defaultNode, showPlaceholderLeaves, showPlaceholderLeavesAdvanced, isNodeClustered, extendConnectorsBy, extendConnectorsByAdvanced, checksum
    )

{-| Various settings for the editor, defined at the time of [initialization](Arborist#initWith), or [added](Arborist#applySettings) at any time later in the program. Includes various geometric settings such as canvas dimensions and the gutter between nodes, and, in a later version of this package, more functional settings such as hiding placeholder nodes.


## Geometry

@docs nodeWidth, nodeHeight, canvasWidth, canvasHeight, level, gutter, centerOffset, connectorStroke, connectorStrokeWidth


## Features

@docs dragAndDrop, keyboardNavigation, keyboardNavigationOutside, defaultNode, showPlaceholderLeaves, showPlaceholderLeavesAdvanced, isNodeClustered, extendConnectorsBy, extendConnectorsByAdvanced, checksum

-}

import Internals.Settings exposing (Setting(..))


{-| Sets the width of node.
-}
nodeWidth : Int -> Setting node
nodeWidth =
    NodeWidth


{-| Sets the width of node.
-}
nodeHeight : Int -> Setting node
nodeHeight =
    NodeHeight


{-| Sets the width of the canvas.
-}
canvasWidth : Int -> Setting node
canvasWidth =
    CanvasWidth


{-| Sets the height of the canvas.
-}
canvasHeight : Int -> Setting node
canvasHeight =
    CanvasHeight


{-| Vertical distance between the bottom of a parent node and the top of its child.
-}
level : Int -> Setting node
level =
    Level


{-| Horizontal gutter between nodes.
-}
gutter : Int -> Setting node
gutter =
    Gutter


{-| When a node is activated, the interface centers it automatically. Sometimes, though, it is useful to move it somewhere other than the center, e.g. to accommodate a large pop-up underneath. With this configuration option, you can specify this offset, horizontal first, vertical second.
-}
centerOffset : Int -> Int -> Setting node
centerOffset =
    CenterOffset


{-| Specifies whether node connectors should extend a certain distance at root- or leaf nodes to connect to peripheral UI elements
-}
extendConnectorsBy : Int -> Setting node
extendConnectorsBy =
    ExtendConnectorsBy


{-| A fine-grained control version of `showPlaceholderLeaves`, allowing control on the display of placeholder leaves based on the contents of the node, as well as its parent and children.
-}
extendConnectorsByAdvanced :
    ({ node : Maybe node
     , parent : Maybe node
     , children : List node
     , siblings : List node
     }
     -> Maybe Int
    )
    -> Setting node
extendConnectorsByAdvanced =
    ExtendConnectorsByAdvanced


{-| Stroke color of the lines connecting siblings.
-}
connectorStroke : String -> Setting node
connectorStroke =
    ConnectorStroke


{-| Stroke width of the lines connecting siblings.
-}
connectorStrokeWidth : String -> Setting node
connectorStrokeWidth =
    ConnectorStrokeWidth


{-| Enable or disable drag and drop with this boolean flag (`True` is enabled, which is the default value). Note that the tree can still be panned if drag and drop is disabled.
-}
dragAndDrop : Bool -> Setting node
dragAndDrop =
    DragAndDrop


{-| Enable or disable keyboard navigation, which traverses and activates nodes using the arrow keys
-}
keyboardNavigation : Bool -> Setting node
keyboardNavigation =
    KeyboardNavigation


{-| Similar to `keyboardNavigation`, but only works if the event is recorded outside a DOM element with the specified ID. Use this to disable keyboard navigation if the user types inside an input.
-}
keyboardNavigationOutside : String -> Bool -> Setting node
keyboardNavigationOutside =
    KeyboardNavigationOutside


{-| Set whether placeholder leaves should be displayed.
-}
showPlaceholderLeaves : Bool -> Setting node
showPlaceholderLeaves =
    ShowPlaceholderLeaves


{-| A fine-grained control version of `showPlaceholderLeaves`, allowing control on the display of placeholder leaves based on the contents of the node, as well as its parent and children.
-}
showPlaceholderLeavesAdvanced :
    ({ node : node
     , parent : Maybe node
     , children : List node
     , siblings : List node
     }
     -> Bool
    )
    -> Setting node
showPlaceholderLeavesAdvanced =
    ShowPlaceholderLeavesAdvanced


{-| Tells arborist which nodes should be clustered. Clustered nodes do not render their children, making the tree easier to understand. Use e.g. [setActive](#setActive) to toggle clustered state on and off.
-}
isNodeClustered : (node -> Bool) -> Setting node
isNodeClustered =
    IsNodeClustered


{-| Sets a default node to be created automatically when a child placeholder is clicked. If none is specified, the placeholder is activated so that a new node may be added manually.
-}
defaultNode : node -> Setting node
defaultNode =
    DefaultNode


{-| A checksum value for settings, allowing the layout algorithm to know when the settings have not meaningfully changed and that a previous computed tree layout can be re-used.

    This is necessary because some settings are functions which cannot be compared directly.

-}
checksum : String -> Setting node
checksum =
    Checksum
