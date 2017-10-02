module Arborist.Tree exposing (Tree(..), decoder, depth, map)

{-| A tiny tiny tree module. Only a few utility methods are provided here - after all, if you want to manupilate the tree, you should probably do so using the interface 🤓.


# Type

@docs Tree


# Json

@docs decoder


# Methods

@docs depth, map

-}

import Json.Decode as Decode


{-| Recursive tree structure, holding any data type `a`, and any number of child nodes.
-}
type Tree item
    = Empty
    | Node item (List (Tree item))


{-| Tree decoder as a function of the node's decoder. Assumes a `value` and `children` fields, holding the current node contents and an array of children, respectively.
-}
decoder : Decode.Decoder item -> Decode.Decoder (Tree item)
decoder nodeDecoder =
    Decode.oneOf
        [ Decode.null Empty
        , Decode.map2 Node
            (Decode.field "value" nodeDecoder)
            (Decode.field "children" (Decode.lazy (\_ -> decoder nodeDecoder |> Decode.list)))
        ]


{-| Tree depth.
-}
depth : Tree item -> Int
depth tree =
    depthHelper 1 tree


depthHelper : Int -> Tree item -> Int
depthHelper currentDepth tree =
    case tree of
        Empty ->
            currentDepth

        Node item children ->
            [ currentDepth ] ++ (List.map (depthHelper (currentDepth + 1)) children) |> List.foldl max -1


{-| Map over the items of the tree.
-}
map : (a -> b) -> Tree a -> Tree b
map fn tree =
    case tree of
        Empty ->
            Empty

        Node val children ->
            Node (fn val) (List.map (map fn) children)
