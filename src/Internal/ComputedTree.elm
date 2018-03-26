module Internal.ComputedTree exposing (ComputedTree, tree, flat, layout, init, item)

{-| This module encapsulates computed information on a tree, information typically used for layout. When the tree is updated, there is a guarantee that this data is recalculated, avoiding expensive render-time recalculations or leaving it up to the developer to make sure the cache doesn't go stale.
-}

import Internal.TreeHelpers as TreeHelpers exposing (TreeNodePath)
import Arborist.Tree


type ComputedTree item
    = ComputedTree
        { tree : Arborist.Tree.Tree item
        , flat : List ( TreeNodePath, Maybe item )
        , layout : TreeHelpers.Layout
        }


tree : ComputedTree item -> Arborist.Tree.Tree item
tree (ComputedTree computedTree) =
    computedTree.tree


flat : ComputedTree item -> List ( TreeNodePath, Maybe item )
flat (ComputedTree { flat }) =
    flat


layout : ComputedTree item -> TreeHelpers.Layout
layout (ComputedTree computedTree) =
    computedTree.layout


item : TreeNodePath -> ComputedTree item -> Maybe item
item path (ComputedTree { flat }) =
    flat
        |> List.filter (\( path_, item ) -> path == path_)
        |> List.head
        |> Maybe.andThen Tuple.second


init : Bool -> Arborist.Tree.Tree item -> ComputedTree item
init showPlaceholderLeaves tree =
    let
        withPlaceholders =
            if showPlaceholderLeaves then
                TreeHelpers.addTrailingEmpties tree
            else
                tree

        flat =
            TreeHelpers.flatten withPlaceholders

        layout =
            withPlaceholders
                |> TreeHelpers.analyze
                |> TreeHelpers.layout
    in
        ComputedTree
            { tree = tree
            , flat = flat
            , layout = layout
            }
