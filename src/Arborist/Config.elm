module Arborist.Config exposing (..)

{-| Configuration and view context for the tree editor.


# Config

@docs Config, Layout, NodeState


# View Context

@docs Context

-}

import Html exposing (Html)
import Messages exposing (Msg)


{-| The state of a node at a given time. May be normal one of the following:

  - `Normal`: node in rest state
  - `Hovered`: a hovered over node
  - `Active`: an activated node. Overrides hover
  - `DropTarget`: indicates that a swap or insert of the dragged subtree will take place at this node upon release

-}
type NodeState
    = Normal
    | Active
    | Hovered
    | DropTarget


{-| View context. Contains the following fields:

  - `parent`: the item at the parent, not available for the root node
  - `siblings`: a list of all direct siblings
  - `state`: node [state](/Arborist-Config#NodeState)

-}
type alias Context item =
    { parent : Maybe item
    , siblings : List item
    , state : NodeState
    }


{-| Layout dimensions for the tree editor, with the following fields:

  - `canvasWidth`: width of the canvas containing the entire editor interface
  - `canvasHeight`: height of the canvas containing the entire editor interface
  - `nodeWidth`: width of a single node
  - `nodeHeight`: height of a single node
  - `level`: vertical offset between different levels of nodes (from the bottom of the parent to the top of the child)
  - `gutter`: horizontal gutter between two sibling nodes

-}
type alias Layout =
    { canvasWidth : Float
    , canvasHeight : Float
    , nodeWidth : Float
    , nodeHeight : Float
    , level : Float
    , gutter : Float
    }


{-| Global configuration for the editor module, consisting of two fields:

  - `view`: the view of a single node function, dependent on its [Context](/Arborist-Config#Context) and the item it holds (may be `Nothing` in case of a placeholder). Since there are all sorts of event handlers on the node element containing this piece of html, the library essentially disallows any messages (it does so by expecting the message type to be of `Msg item`, but does not actually expose any `Msg` constructors)
  - `layout`: bits of geometry information, detailed [here](/Arborist-Context#Layout)

-}
type alias Config item =
    { view : Context item -> Maybe item -> Html (Msg item)
    , layout : Layout
    }
