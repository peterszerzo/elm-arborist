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
    Dict.Dict comparable { center : ( Float, Float ), span : Float }


type alias TreeAnalysis comparable =
    Dict.Dict comparable { path : List Int, siblings : Int, children : List comparable }



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
            [ Dict.singleton (toId item)
                { path = path
                , siblings = siblings
                , children =
                    List.map
                        (\child ->
                            case child of
                                Node item _ ->
                                    Just (toId item)

                                Empty ->
                                    Nothing
                        )
                        children
                        |> List.filterMap identity
                }
            ]
                ++ (List.indexedMap
                        (\index item ->
                            analyzeTail
                                { path = path ++ [ index ]
                                , siblings = List.length children - 1
                                }
                                toId
                                item
                        )
                        children
                   )
                |> List.foldl Dict.union Dict.empty


compareLists : List comparable -> List comparable -> Order
compareLists l1 l2 =
    case ( l1, l2 ) of
        ( head1 :: tail1, head2 :: tail2 ) ->
            let
                comp =
                    compare head1 head2
            in
                if comp == EQ then
                    compareLists tail1 tail2
                else
                    comp

        ( _, _ ) ->
            EQ


{-| Lays out elements.
-}
layout : Int -> TreeAnalysis comparable -> TreeLayout comparable
layout showLevels analysis =
    let
        layout =
            Dict.empty

        displayedItems =
            analysis
                |> Dict.filter (\id { path } -> List.length path < showLevels + 1)

        lowestLevelItems =
            displayedItems
                |> Dict.filter (\_ a -> List.length a.path == showLevels - 1)
                |> Dict.toList

        lowestLevelPass =
            lowestLevelItems
                |> List.sortWith
                    (\( _, a1 ) ( _, a2 ) ->
                        compareLists a1.path a2.path
                    )
                |> List.indexedMap
                    (\index ( id, _ ) ->
                        ( id
                        , { center =
                                ( toFloat index - (toFloat (List.length lowestLevelItems - 1)) / 2
                                , 2
                                )
                          , span = 0
                          }
                        )
                    )
                |> Dict.fromList

        secondLevelPass =
            layoutLevelPass lowestLevelPass 1 displayedItems

        thirdLevelPass =
            layoutLevelPass secondLevelPass 0 displayedItems
    in
        [ lowestLevelPass, secondLevelPass, thirdLevelPass ]
            |> List.foldl Dict.union Dict.empty
            |> Debug.log "layout"


layoutLevelPass : TreeLayout comparable -> Int -> TreeAnalysis comparable -> TreeLayout comparable
layoutLevelPass previousPass level nodeAnalysisRes =
    nodeAnalysisRes
        |> Dict.filter (\_ a -> List.length a.path == level)
        |> Dict.toList
        |> List.map
            (\( id, { children } ) ->
                ( id
                , List.map
                    (\child ->
                        Dict.get child previousPass
                            |> Maybe.withDefault { center = ( 0, 0 ), span = 0 }
                    )
                    children
                    |> (\list ->
                            let
                                centers =
                                    List.map (.center >> Tuple.first) list
                            in
                                { center =
                                    ( (List.foldl (+) 0 centers)
                                        / (List.length centers |> toFloat)
                                    , toFloat level
                                    )
                                , span =
                                    Maybe.map2 (\max min -> max - min)
                                        (List.maximum centers)
                                        (List.minimum centers)
                                        |> Maybe.withDefault 0
                                }
                       )
                )
            )
        |> Dict.fromList
