module Arborist.Settings
    exposing
        ( Settings
        , nodeWidth
        , nodeHeight
        , canvasWidth
        , canvasHeight
        , level
        , gutter
        , centerOffset
        )

{-| Configuration and view context for the tree editor.


# Settings object

@docs Settings


# Settings

@docs nodeWidth, nodeHeight, canvasWidth, canvasHeight, level, gutter, centerOffset

-}


{-| Layout dimensions for the tree editor, with the following fields:

  - `canvasWidth`: width of the canvas containing the entire editor interface
  - `canvasHeight`: height of the canvas containing the entire editor interface
  - `nodeWidth`: width of a single node
  - `nodeHeight`: height of a single node
  - `level`: vertical offset between different levels of nodes (from the bottom of the parent to the top of the child)
  - `gutter`: horizontal gutter between two sibling nodes

-}
type alias Settings =
    { canvasWidth : Float
    , canvasHeight : Float
    , nodeWidth : Float
    , nodeHeight : Float
    , level : Float
    , gutter : Float
    , centerOffset : ( Float, Float )
    }


type Option
    = NodeWidth Int
    | NodeHeight Int
    | CanvasWidth Int
    | CanvasHeight Int
    | Level Int
    | Gutter Int
    | CenterOffset Int Int


construct : List Option -> Settings -> Settings
construct options layout =
    case options of
        [] ->
            layout

        head :: tail ->
            construct tail
                (case head of
                    NodeWidth w ->
                        { layout | nodeWidth = toFloat w }

                    NodeHeight h ->
                        { layout | nodeHeight = toFloat h }

                    CanvasWidth w ->
                        { layout | canvasWidth = toFloat w }

                    CanvasHeight h ->
                        { layout | canvasHeight = toFloat h }

                    Level l ->
                        { layout | level = toFloat l }

                    Gutter g ->
                        { layout | gutter = toFloat g }

                    CenterOffset x y ->
                        { layout | centerOffset = ( toFloat x, toFloat y ) }
                )


{-| Sets the width of a single node.
-}
nodeWidth : Int -> Option
nodeWidth =
    NodeWidth


{-| Sets the width of a single node.
-}
nodeHeight : Int -> Option
nodeHeight =
    NodeHeight


{-| Sets the width of a single canvas.
-}
canvasWidth : Int -> Option
canvasWidth =
    CanvasWidth


{-| Sets the width of a single canvas.
-}
canvasHeight : Int -> Option
canvasHeight =
    CanvasHeight


{-| Vertical height of a level, between the bottom of the upper node and the top of the lower one.
-}
level : Int -> Option
level =
    Level


{-| Horizontal gutter between nodes.
-}
gutter : Int -> Option
gutter =
    Gutter


{-| When a node is activated, the interface centers it automatically. Sometimes, though, it is useful to move it somewhere other than the center, e.g. to accommodate a large pop-up. With this configuration option, you can specify this dimension by passing a horizontal offset first and a vertical one second.
-}
centerOffset : Int -> Int -> Option
centerOffset =
    CenterOffset
