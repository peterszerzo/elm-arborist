module Internal.TreeHelpers exposing (..)

import Dict
import Json.Decode as Decode
import Utils
import Arborist.Tree as Tree exposing (..)


-- Data structure


type alias TreeNodePath =
    List Int



-- Calculation types


type alias Layout =
    Dict.Dict TreeNodePath
        { center : ( Float, Float )
        , childCenters : List Float
        }


type alias NodeInfo =
    Dict.Dict TreeNodePath
        { siblings : Int
        , children : List TreeNodePath
        }


type alias TreeAnalysis =
    { depth : Int
    , shortNodes : List TreeNodePath
    , nodeInfo : NodeInfo
    }


{-| Add an empty element every
-}
addTrailingEmpties : Tree a -> Tree a
addTrailingEmpties tree =
    case tree of
        Tree.Empty ->
            Empty

        Tree.Node item children ->
            Node item <| (List.map addTrailingEmpties children) ++ [ Empty ]


removeEmpties : Tree a -> Tree a
removeEmpties tree =
    case tree of
        Empty ->
            Empty

        Node item children ->
            Node item <| (List.filter (\tree -> tree /= Empty) children |> List.map removeEmpties)


{-| Find item by path
-}
find : TreeNodePath -> Tree a -> Maybe (Tree a)
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


swap : TreeNodePath -> TreeNodePath -> Tree a -> Tree a
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


updateSubtree : TreeNodePath -> Tree a -> Tree a -> Tree a
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

        ( [], _ ) ->
            subtree

        ( _, tree ) ->
            tree


update : TreeNodePath -> a -> Tree a -> Tree a
update path replaceItem tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            Node item <|
                (List.indexedMap
                    (\index child ->
                        if index == head then
                            update tail replaceItem child
                        else
                            child
                    )
                    children
                )

        ( [], Node item children ) ->
            Node replaceItem children

        ( _, tree ) ->
            tree


insert : TreeNodePath -> Maybe a -> Tree a -> Tree a
insert path insertItem tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            Node item <|
                List.indexedMap
                    (\index child ->
                        if index == head then
                            insert tail insertItem child
                        else
                            child
                    )
                    children

        ( [], Node item children ) ->
            Node item
                (children
                    ++ [ insertItem
                            |> Maybe.map (\i -> Node i [])
                            |> Maybe.withDefault Empty
                       ]
                )

        ( _, tree ) ->
            tree


delete : TreeNodePath -> Tree a -> Tree a
delete path tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            children
                |> List.indexedMap
                    (\index child ->
                        if index == head then
                            (delete tail child)
                        else
                            child
                    )
                |> List.filter ((/=) Empty)
                |> Node item

        ( [], Node _ _ ) ->
            Empty

        ( _, tree ) ->
            tree


{-| Flatten
-}
flatten : Tree a -> List ( TreeNodePath, Maybe a )
flatten =
    flattenTail []


flattenTail : TreeNodePath -> Tree a -> List ( TreeNodePath, Maybe a )
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


{-| Analyze tree.
-}
analyze : Tree a -> TreeAnalysis
analyze tree =
    analyzeTail { depth = depth tree, current = { path = [], siblings = 0 } } tree


analyzeTail : { depth : Int, current : { path : TreeNodePath, siblings : Int } } -> Tree a -> TreeAnalysis
analyzeTail { depth, current } tree =
    case tree of
        Empty ->
            { depth = depth
            , nodeInfo = Dict.singleton current.path { siblings = current.siblings, children = [] }
            , shortNodes =
                if List.length current.path < depth - 1 then
                    [ current.path ]
                else
                    []
            }

        Node item children ->
            let
                childrenAnalysis =
                    List.indexedMap
                        (\index child ->
                            analyzeTail
                                { depth = depth
                                , current =
                                    { path = current.path ++ [ index ]
                                    , siblings = List.length children - 1
                                    }
                                }
                                child
                        )
                        children
            in
                { depth = depth
                , nodeInfo =
                    [ Dict.singleton current.path
                        { siblings = current.siblings
                        , children = List.indexedMap (\index _ -> current.path ++ [ index ]) children
                        }
                    ]
                        ++ (List.map .nodeInfo childrenAnalysis)
                        |> List.foldl Dict.union Dict.empty
                , shortNodes = childrenAnalysis |> List.map .shortNodes |> List.foldl (++) []
                }


{-| Lays out elements.
-}
layout : TreeAnalysis -> Layout
layout analysis =
    let
        showLevels =
            analysis.depth

        shortNodeFillerNodeInfo =
            List.map
                (\path ->
                    let
                        depthDiff =
                            (analysis.depth - (List.length path) - 1)

                        extraNodes =
                            List.range 0 (depthDiff - 1)
                                |> List.map
                                    (\n ->
                                        let
                                            childPath =
                                                path ++ (List.range 0 n |> List.map (always 0))
                                        in
                                            { path = childPath
                                            , siblings = 0
                                            , children =
                                                if (n == depthDiff - 1) then
                                                    []
                                                else
                                                    [ childPath ++ [ 0 ] ]
                                            }
                                    )
                    in
                        extraNodes
                )
                analysis.shortNodes
                |> List.foldl (++) []
                |> List.map (\{ path, siblings, children } -> ( path, { siblings = siblings, children = children } ))
                |> Dict.fromList

        placeholderAdjustedAnalysisNodeInfo =
            analysis.nodeInfo
                |> Dict.map
                    (\path info ->
                        if (List.member path analysis.shortNodes) then
                            { info | children = [ path ++ [ 0 ] ] }
                        else
                            info
                    )

        nodeInfo =
            Dict.union placeholderAdjustedAnalysisNodeInfo shortNodeFillerNodeInfo

        lowestLevelItems =
            nodeInfo
                |> Dict.filter (\path _ -> List.length path == showLevels - 1)
                |> Dict.toList

        layout =
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
                                , (toFloat showLevels) - 1
                                )
                          , childCenters = []
                          }
                        )
                    )
                |> Dict.fromList
                |> layoutLevelPass (showLevels - 2) nodeInfo
    in
        layout


layoutLevelPass : Int -> NodeInfo -> Layout -> Layout
layoutLevelPass level nodeInfo layout =
    case level of
        (-1) ->
            layout

        level ->
            nodeInfo
                |> Dict.filter (\path _ -> List.length path == level)
                |> Dict.toList
                |> List.map
                    (\( path, { children } ) ->
                        ( path
                        , List.map
                            (\child ->
                                Dict.get child layout
                                    |> Maybe.withDefault
                                        { center = ( 0, 0 )
                                        , childCenters = []
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
                                        , childCenters = centers
                                        }
                               )
                        )
                    )
                |> Dict.fromList
                |> (\pass -> layoutLevelPass (level - 1) nodeInfo (Dict.union layout pass))
