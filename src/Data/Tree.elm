module Data.Tree exposing (..)

import Set
import Dict
import Json.Decode as Decode


-- Data structure


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



-- Calculation types


type alias TreeLayout comparable =
    Dict.Dict comparable ( Float, Float )


type alias TreeAnalysis comparable =
    Dict.Dict comparable { path : List Int, siblings : Int }



-- Update


{-| Return an empty tree.
-}
empty : Tree a
empty =
    Empty


{-| Return a tree with a single element.
-}
singleton : a -> Tree a
singleton item =
    Node item []


{-| Map over the items of the tree.
-}
map : (a -> b) -> Tree a -> Tree b
map fn tree =
    case tree of
        Empty ->
            Empty

        Node val children ->
            Node (fn val) (List.map (map fn) children)


{-| Find all items matching a value.
-}
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


{-| Find a single value.
-}
findOne : (a -> Bool) -> Tree a -> Maybe a
findOne fn tree =
    find fn tree |> List.head


{-| Insert a tree into a tree (use singleton for single element insert). The new item is always inserted at the end of the list - reordering happens with drag and drop.
-}
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


{-| Remove item from the tree.
-}
remove : (a -> Bool) -> Tree a -> Tree a
remove findId tree =
    case tree of
        Empty ->
            Empty

        Node item children ->
            if findId item then
                Empty
            else
                Node item (List.map (remove findId) children |> List.filter (\tree -> tree /= Empty))


{-| Find a subtree, also returning its parent.
-}
findOneSubtreeWithParent : (a -> Bool) -> Tree a -> Maybe ( Tree a, Maybe a )
findOneSubtreeWithParent =
    findOneSubtreeWithParentTail Nothing


findOneSubtreeWithParentTail : Maybe a -> (a -> Bool) -> Tree a -> Maybe ( Tree a, Maybe a )
findOneSubtreeWithParentTail parent fn tree =
    case tree of
        Empty ->
            Nothing

        Node val children ->
            if fn val then
                Just ( Node val children, parent )
            else
                List.map (findOneSubtreeWithParentTail (Just val) fn) children
                    |> List.filterMap identity
                    |> List.head


{-| Swap a single item. Leave subtrees in place.
-}
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


{-| Verifies that the members of the tree have unique ids each.
-}
ids : (a -> comparable) -> Tree a -> Set.Set comparable
ids toId tree =
    case tree of
        Empty ->
            Set.empty

        Node item children ->
            [ Set.singleton (toId item) ]
                ++ (List.map (ids toId) children)
                |> List.foldl Set.union Set.empty


{-| Returns a dictionary ordered by item identifiers.
-}
analyze : (a -> comparable) -> Tree a -> TreeAnalysis comparable
analyze =
    analyzeTail { path = [], siblings = 0 }


analyzeTail : { path : List Int, siblings : Int } -> (a -> comparable) -> Tree a -> TreeAnalysis comparable
analyzeTail { path, siblings } toId tree =
    case tree of
        Empty ->
            Dict.empty

        Node item children ->
            [ Dict.singleton (toId item) { path = path, siblings = siblings } ]
                ++ (List.indexedMap
                        (\index item -> analyzeTail { path = path ++ [ index ], siblings = List.length children - 1 } toId item)
                        children
                   )
                |> List.foldl Dict.union Dict.empty


{-| Lays out elements.
-}
layout : Int -> TreeAnalysis comparable -> TreeLayout comparable
layout showLevels analysis =
    let
        displayedItems =
            analysis
                |> Dict.filter (\id { path } -> List.length path < showLevels)
                |> Debug.log "di"

        lowestLevelPass =
            displayedItems
                |> Dict.filter (\id a -> List.length a.path == showLevels - 1)
                |> Debug.log "lowest"
    in
        displayedItems |> Dict.map (\id _ -> ( 0, 0 ))
