module Data.ComputedTree exposing (ComputedTree, tree, flat, layout, init, item)

{-| This module encapsulates computed information on a tree, information typically used for layout. When the tree is updated, there is a guarantee that this data is recalculated, avoiding expensive render-time recalculations or leaving it up to the developer to make sure a cache doesn't go stale.
-}

import Utils.Tree as Tree exposing (TreeNodePath)
import Arborist.Tree


type ComputedTree item
    = ComputedTree
        { tree : Arborist.Tree.Tree item
        , flat : List ( TreeNodePath, Maybe item )
        , layout : Tree.Layout
        }


tree : ComputedTree item -> Arborist.Tree.Tree item
tree (ComputedTree computedTree) =
    computedTree.tree


flat : ComputedTree item -> List ( TreeNodePath, Maybe item )
flat (ComputedTree { flat }) =
    flat


layout : ComputedTree item -> Tree.Layout
layout (ComputedTree computedTree) =
    computedTree.layout


item : TreeNodePath -> ComputedTree item -> Maybe item
item path (ComputedTree { flat }) =
    flat
        |> List.filter (\( path_, item ) -> path == path_)
        |> List.head
        |> Maybe.andThen Tuple.second


init : Arborist.Tree.Tree item -> ComputedTree item
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
