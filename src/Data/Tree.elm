module Data.Tree exposing (..)

import Dict
import Json.Decode as Decode
import Utils


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


type alias Analysis =
    { siblings : Int, children : List (List Int) }


type alias TreeLayout =
    Dict.Dict (List Int) { center : ( Float, Float ), span : Float, children : Int }


type alias TreeAnalysis =
    Dict.Dict (List Int) Analysis



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


{-| Add an empty element every
-}
addTrailingEmpties : Tree a -> Tree a
addTrailingEmpties tree =
    case tree of
        Empty ->
            Empty

        Node item children ->
            Node item <| (List.map addTrailingEmpties children) ++ [ Empty ]


{-| Find item by path
-}
find : List Int -> Tree a -> Maybe (Tree a)
find path tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            children
                |> List.drop head
                |> List.head
                |> Maybe.andThen (\childTree -> find tail childTree)

        ( [], tree ) ->
            Just tree

        ( head :: tail, Empty ) ->
            Nothing


swap : List Int -> List Int -> Tree a -> Tree a
swap path1 path2 tree =
    let
        subtree1 =
            find path1 tree

        subtree2 =
            find path2 tree
    in
        Maybe.map2
            (\st1 st2 ->
                tree
                    |> updateSubtree path1 st2
                    |> updateSubtree path2 st1
            )
            subtree1
            subtree2
            |> Maybe.withDefault tree


updateSubtree : List Int -> Tree a -> Tree a -> Tree a
updateSubtree path subtree tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            Node item <|
                List.indexedMap
                    (\index child ->
                        if index == head then
                            updateSubtree tail subtree child
                        else
                            child
                    )
                    children

        ( [], Node item children ) ->
            subtree

        ( _, tree ) ->
            tree


update : List Int -> a -> Tree a -> Tree a
update path replaceItem tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            Node item <|
                List.indexedMap
                    (\index child ->
                        if index == head then
                            update tail replaceItem child
                        else
                            child
                    )
                    children

        ( [], Node item children ) ->
            Node replaceItem children

        ( _, tree ) ->
            tree


delete : List Int -> Tree a -> Tree a
delete path tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            children
                |> List.map (delete tail)
                |> List.filter (\child -> child /= Empty)
                |> Node item

        ( [], Node _ _ ) ->
            Empty

        ( _, tree ) ->
            tree



-- (head :: tail, Empty) ->


{-| Flatten
-}
flatten : Tree a -> List ( List Int, Maybe a )
flatten =
    flattenTail []


flattenTail : List Int -> Tree a -> List ( List Int, Maybe a )
flattenTail path tree =
    case tree of
        Empty ->
            [ ( path, Nothing ) ]

        Node val children ->
            [ ( path, Just val ) ]
                ++ (List.indexedMap
                        (\index child ->
                            (flattenTail (path ++ [ index ]) child)
                        )
                        children
                        |> List.foldl (++) []
                   )


{-| Returns a dictionary ordered by item identifiers.
-}
analyze : Tree a -> TreeAnalysis
analyze =
    analyzeTail { path = [], siblings = 0 }


analyzeTail : { path : List Int, siblings : Int } -> Tree a -> TreeAnalysis
analyzeTail { path, siblings } tree =
    case tree of
        Empty ->
            Dict.singleton path { siblings = siblings, children = [] }

        Node item children ->
            [ Dict.singleton path
                { siblings = siblings
                , children = List.indexedMap (\index _ -> path ++ [ index ]) children
                }
            ]
                ++ (List.indexedMap
                        (\index child ->
                            analyzeTail
                                { path = path ++ [ index ]
                                , siblings = List.length children - 1
                                }
                                child
                        )
                        children
                   )
                |> List.foldl Dict.union Dict.empty


{-| Lays out elements.
-}
layout : Int -> TreeAnalysis -> TreeLayout
layout showLevels analysis =
    let
        layout =
            Dict.empty

        displayedItems =
            analysis
                |> Dict.filter (\path _ -> List.length path < showLevels + 1)

        lowestLevelItems =
            displayedItems
                |> Dict.filter (\path _ -> List.length path == showLevels - 1)
                |> Dict.toList

        lowestLevelPass =
            lowestLevelItems
                |> List.sortWith
                    (\( path1, _ ) ( path2, _ ) ->
                        Utils.compareLists path1 path2
                    )
                |> List.indexedMap
                    (\index ( id, _ ) ->
                        ( id
                        , { center =
                                ( toFloat index - (toFloat (List.length lowestLevelItems - 1)) / 2
                                , 2
                                )
                          , span = 0
                          , children = 0
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


layoutLevelPass : TreeLayout -> Int -> TreeAnalysis -> TreeLayout
layoutLevelPass previousPass level nodeAnalysisRes =
    nodeAnalysisRes
        |> Dict.filter (\path _ -> List.length path == level)
        |> Dict.toList
        |> List.map
            (\( path, { children } ) ->
                ( path
                , List.map
                    (\child ->
                        Dict.get child previousPass
                            |> Maybe.withDefault
                                { center = ( 0, 0 )
                                , span = 0
                                , children = List.length children
                                }
                    )
                    children
                    |> (\list ->
                            let
                                centers =
                                    List.map (.center >> Tuple.first) list

                                centerX =
                                    if (List.length centers == 0) then
                                        0
                                    else
                                        (List.foldl (+) 0 centers)
                                            / (List.length centers |> toFloat)
                            in
                                { center =
                                    ( centerX
                                    , toFloat level
                                    )
                                , span =
                                    Maybe.map2 (\max min -> max - min)
                                        (List.maximum centers)
                                        (List.minimum centers)
                                        |> Maybe.withDefault 0
                                , children = List.length children
                                }
                       )
                )
            )
        |> Dict.fromList
