module Arborist.Tree exposing (Tree(..), decoder, encoder, flatten, depth, map)

{-| A tiny tiny tree module. Only a few utility methods are provided here - after all, if you want to manupilate the tree, you should probably do so using the interface 🤓.


# Type

@docs Tree


# Json

@docs decoder, encoder


# Methods

@docs depth, flatten, map

-}

import Json.Decode as Decode
import Json.Encode as Encode


{-| Recursive tree structure, holding any data type `node`, and any number of child nodes. Creating a tree of strings, for instance, would look like this:

    Tree.Node "Parent" [ Tree.Node "Child1" [], Tree.Node "Child2" [] ]

-}
type Tree node
    = Empty
    | Node node (List (Tree node))


{-| Tree decoder as a function of the node's decoder. Assumes a `value` and `children` fields, holding the current node contents and an array of children, respectively.
-}
decoder : Decode.Decoder node -> Decode.Decoder (Tree node)
decoder nodeDecoder =
    Decode.oneOf
        [ Decode.null Empty
        , Decode.map2 Node
            (Decode.field "value" nodeDecoder)
            (Decode.field "children" (Decode.lazy (\_ -> decoder nodeDecoder |> Decode.list)))
        ]


{-| Encodes a tree into JSON, given its node encoder.
-}
encoder : (node -> Encode.Value) -> Tree node -> Encode.Value
encoder nodeEncoder tree =
    case tree of
        Empty ->
            Encode.null

        Node node children ->
            Encode.object
                [ ( "value", nodeEncoder node )
                , ( "children", List.map (encoder nodeEncoder) children |> Encode.list )
                ]


{-| Tree depth.
-}
depth : Tree node -> Int
depth tree =
    depthHelper 1 tree


depthHelper : Int -> Tree node -> Int
depthHelper currentDepth tree =
    case tree of
        Empty ->
            currentDepth

        Node _ children ->
            currentDepth :: List.map (depthHelper (currentDepth + 1)) children |> List.foldl max -1


{-| Map over the nodes of the tree.
-}
map : (a -> b) -> Tree a -> Tree b
map fn tree =
    case tree of
        Empty ->
            Empty

        Node val children ->
            Node (fn val) (List.map (map fn) children)


{-| Flatten a tree into a list of ( path, node ) tuples. The path is a list of integers showing how you can get to the node (the root would be `[]`, its first child `[ 1 ]`).
-}
flatten : Tree a -> List ( List Int, a )
flatten =
    flattenTail []


flattenTail : List Int -> Tree a -> List ( List Int, a )
flattenTail path tree =
    case tree of
        Empty ->
            []

        Node val children ->
            [ ( path, val ) ]
                ++ (List.indexedMap
                        (\index child ->
                            flattenTail (path ++ [ index ]) child
                        )
                        children
                        |> List.foldl (++) []
                   )
