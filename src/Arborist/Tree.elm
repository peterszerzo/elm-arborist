module Arborist.Tree exposing
    ( Tree(..)
    , decoder, encoder
    , depth, flatten, map
    )

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
-}
type Tree node
    = Empty
    | Node node (List (Tree node))


{-| Decode a tree based on the decoder of its node
-}
decoder : Decode.Decoder node -> Decode.Decoder (Tree node)
decoder nodeDecoder =
    Decode.oneOf
        [ Decode.null Empty
        , Decode.map2 Node
            (Decode.field "value" nodeDecoder)
            (Decode.field "children" (Decode.lazy (\_ -> decoder nodeDecoder |> Decode.list)))
        ]


{-| Encodes a tree based on the encoder of its node
-}
encoder : (node -> Encode.Value) -> Tree node -> Encode.Value
encoder nodeEncoder tree =
    case tree of
        Empty ->
            Encode.null

        Node node children ->
            Encode.object
                [ ( "value", nodeEncoder node )
                , ( "children", Encode.list (encoder nodeEncoder) children )
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
            ( path, val )
                :: (List.indexedMap
                        (\index child ->
                            flattenTail (path ++ [ index ]) child
                        )
                        children
                        |> List.foldl (++) []
                   )
