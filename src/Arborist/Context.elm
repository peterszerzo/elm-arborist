module Arborist.Context exposing (..)

{-| This module defines the node view context, an object passed to the [NodeView](#NodeView) function responsible for rendering individual nodes within a tree's layout. It holds information such as the node's parent, siblings, hover state etc.

Effective tree editors need to give the users feedback on whether the node they're about to create is valid, or if it creates conflicts with its surrounding nodes based on your domain's rules. Imagine modeling a conversation as a tree structure, where a set of sibling nodes answer a question represented in the common parent. Would you allow two nodes representing the answer 'no'? By having access to the context when defining how nodes should look, you can render these error messages flexibly to the user.

The best part: at no point will you have to think about traversing a recursive tree structure. Just work with a simple summary record handed down to you in context.

@docs Context, NodeState

-}


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
  - `children`: a list of all direct children
  - `state`: node [state](Arborist-Settings#NodeState)

-}
type alias Context item =
    { parent : Maybe item
    , siblings : List item
    , children : List item
    , state : NodeState
    }
