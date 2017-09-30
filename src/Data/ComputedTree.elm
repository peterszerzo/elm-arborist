module Data.ComputedTree exposing (ComputedTree, tree, flat, layout, init)

{-| This module encapsulates computed information on a tree, information typically used for layout. When the tree is updated, there is a guarantee that this data is recalculated, avoiding expensive render-time recalculations or leaving it up to the developer to make sure a cache doesn't go stale.
-}

import Data.Tree as Tree


type ComputedTree item
    = ComputedTree
        { tree : Tree.Tree item
        , flat : List ( Tree.TreeNodePath, Maybe item )
        , layout : Tree.Layout
        }


tree : ComputedTree item -> Tree.Tree item
tree (ComputedTree computedTree) =
    computedTree.tree


flat : ComputedTree item -> List ( Tree.TreeNodePath, Maybe item )
flat (ComputedTree { flat }) =
    flat


layout : ComputedTree item -> Tree.Layout
layout (ComputedTree computedTree) =
    computedTree.layout


init : Tree.Tree item -> ComputedTree item
init tree =
    let
        withPlaceholders =
            Tree.addTrailingEmpties tree

        flat =
            Tree.flatten withPlaceholders

        layout =
            withPlaceholders
                |> Tree.analyze
                |> Tree.layout
    in
        ComputedTree
            { tree = tree
            , flat = flat
            , layout = layout
            }
