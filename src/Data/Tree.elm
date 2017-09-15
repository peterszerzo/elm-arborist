module Data.Tree exposing (..)

import Set
import Json.Decode as Decode


type Tree a
    = Empty
    | Node a (List (Tree a))


decoder : Decode.Decoder a -> Decode.Decoder (Tree a)
decoder nodeDecoder =
    Decode.oneOf
        [ Decode.null Empty
        , Decode.map2 Node
            (Decode.field "value" nodeDecoder)
            (Decode.field "children" (Decode.lazy (\_ -> decoder nodeDecoder |> Decode.list)))
        ]



-- Update


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton item =
    Node item []


map : (a -> b) -> Tree a -> Tree b
map fn tree =
    case tree of
        Empty ->
            Empty

        Node val children ->
            Node (fn val) (List.map (map fn) children)


find : (a -> Bool) -> Tree a -> List a
find fn tree =
    case tree of
        Empty ->
            []

        Node val children ->
            (if fn val then
                [ val ]
             else
                []
            )
                ++ (List.map (find fn) children |> List.foldl (++) [])


findOne : (a -> Bool) -> Tree a -> Maybe a
findOne fn tree =
    find fn tree |> List.head


swapOne : (a -> Bool) -> (a -> Bool) -> Tree a -> Tree a
swapOne fn1 fn2 tree =
    let
        item1 =
            findOne fn1 tree

        item2 =
            findOne fn2 tree
    in
        case ( item1, item2 ) of
            ( Just item1, Just item2 ) ->
                map
                    (\item ->
                        if item == item1 then
                            item2
                        else if item == item2 then
                            item1
                        else
                            item
                    )
                    tree

            ( _, _ ) ->
                tree


insert : (a -> Bool) -> Tree a -> Tree a -> Tree a
insert parentFindFn insertedTree tree =
    case tree of
        Empty ->
            tree

        Node item children ->
            if parentFindFn item then
                Node item (children ++ [ insertedTree ])
            else
                Node item (List.map (insert parentFindFn insertedTree) children)


uniqueIdsTail : Set.Set comparable -> (a -> comparable) -> Tree a -> Bool
uniqueIdsTail set getId tree =
    case tree of
        Empty ->
            True

        Node item children ->
            let
                id =
                    getId item

                isMember =
                    Set.member id set

                newSet =
                    Set.insert id set
            in
                ([ not isMember ] ++ (List.map (uniqueIdsTail newSet getId) children))
                    |> List.all identity


{-| Verifies that the members of the tree have unique ids each.
-}
uniqueIds : (a -> comparable) -> Tree a -> Bool
uniqueIds =
    uniqueIdsTail Set.empty
