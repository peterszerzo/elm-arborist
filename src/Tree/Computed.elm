module Tree.Computed exposing (ComputedTree, tree, flat, layout, init, item)

{-| This module encapsulates computed information on a tree, information typically used for layout. When the tree is updated, there is a guarantee that this data is recalculated, avoiding expensive render-time recalculations or leaving it up to the developer to make sure the cache doesn't go stale.
-}

import Tree.Extra exposing (TreeNodePath)
import Tree


type ComputedTree item
    = ComputedTree
        { tree : Tree.Tree item
        , flat : List ( TreeNodePath, Maybe item )
        , layout : Tree.Extra.Layout
        }


tree : ComputedTree item -> Tree.Tree item
tree (ComputedTree computedTree) =
    computedTree.tree


flat : ComputedTree item -> List ( TreeNodePath, Maybe item )
flat (ComputedTree { flat }) =
    flat


layout : ComputedTree item -> Tree.Extra.Layout
layout (ComputedTree computedTree) =
    computedTree.layout


item : TreeNodePath -> ComputedTree item -> Maybe item
item path (ComputedTree { flat }) =
    flat
        |> List.filter (\( path_, item ) -> path == path_)
        |> List.head
        |> Maybe.andThen Tuple.second


init : Bool -> Tree.Tree item -> ComputedTree item
init showPlaceholderLeaves tree =
    let
        withPlaceholders =
            if showPlaceholderLeaves then
                Tree.Extra.addTrailingEmpties tree
            else
                tree

        flat =
            Tree.Extra.flatten withPlaceholders

        layout =
            withPlaceholders
                |> Tree.Extra.analyze
                |> Tree.Extra.layout
    in
        ComputedTree
            { tree = tree
            , flat = flat
            , layout = layout
            }
