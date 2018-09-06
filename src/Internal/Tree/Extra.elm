module Internal.Tree.Extra exposing (Layout, NodeInfo, TreeAnalysis, TreeNodePath, addTrailingEmpties, addTrailingEmptiesAdvanced, analyze, delete, find, flatten, insert, layout, removeEmpties, swap, updateAt, updateAtWithChildren, updateSubtree)

import Arborist.Tree exposing (..)
import Dict
import Utils



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


{-| Add a placeholder element used to add new nodes to the tree
-}
addTrailingEmpties : Tree a -> Tree a
addTrailingEmpties =
    addTrailingEmptiesAdvanced (\_ -> True)


addTrailingEmptiesAdvancedHelper :
    { parent : Maybe a, siblings : List a }
    -> ({ node : a, parent : Maybe a, siblings : List a, children : List a } -> Bool)
    -> Tree a
    -> Tree a
addTrailingEmptiesAdvancedHelper context addEmpty tree =
    case tree of
        Empty ->
            Empty

        Node item children ->
            let
                childNodes =
                    List.map
                        (\child ->
                            case child of
                                Node currentItem _ ->
                                    Just currentItem

                                Empty ->
                                    Nothing
                        )
                        children
                        |> List.filterMap (\x -> x)
            in
            Node item <|
                List.map
                    (addTrailingEmptiesAdvancedHelper
                        { parent = Just item
                        , siblings = childNodes
                        }
                        addEmpty
                    )
                    children
                    ++ (if
                            addEmpty
                                { parent = context.parent
                                , siblings = context.siblings
                                , node = item
                                , children = childNodes
                                }
                        then
                            [ Empty ]

                        else
                            []
                       )


addTrailingEmptiesAdvanced : ({ node : a, parent : Maybe a, siblings : List a, children : List a } -> Bool) -> Tree a -> Tree a
addTrailingEmptiesAdvanced =
    addTrailingEmptiesAdvancedHelper { parent = Nothing, siblings = [] }


removeEmpties : Tree a -> Tree a
removeEmpties tree =
    case tree of
        Empty ->
            Empty

        Node item children ->
            Node item <| (List.filter (\childTree -> childTree /= Empty) children |> List.map removeEmpties)


{-| Find item by path
-}
find : TreeNodePath -> Tree a -> Maybe (Tree a)
find path tree =
    case ( path, tree ) of
        ( head :: tail, Node _ children ) ->
            children
                |> List.drop head
                |> List.head
                |> Maybe.andThen (\childTree -> find tail childTree)

        ( [], currentTree ) ->
            Just currentTree

        ( _ :: _, Empty ) ->
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

        ( _, currentTree ) ->
            currentTree


updateAtWithChildren : TreeNodePath -> a -> Maybe (List a) -> Tree a -> Tree a
updateAtWithChildren path replaceItem replaceChildren tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            Node item <|
                List.indexedMap
                    (\index child ->
                        if index == head then
                            updateAtWithChildren tail replaceItem replaceChildren child

                        else
                            child
                    )
                    children

        ( [], Node _ children ) ->
            Node replaceItem
                (replaceChildren
                    |> Maybe.map (List.map (\child -> Node child []))
                    |> Maybe.withDefault children
                )

        ( _, anyTree ) ->
            anyTree


updateAt : TreeNodePath -> a -> Tree a -> Tree a
updateAt path replaceItem tree =
    updateAtWithChildren path replaceItem Nothing tree


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

        ( _, anyTree ) ->
            anyTree


delete : TreeNodePath -> Tree a -> Tree a
delete path tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            children
                |> List.indexedMap
                    (\index child ->
                        if index == head then
                            delete tail child

                        else
                            child
                    )
                |> List.filter ((/=) Empty)
                |> Node item

        ( [], Node _ _ ) ->
            Empty

        ( _, anyTree ) ->
            anyTree


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
            ( path, Just val )
                :: (List.indexedMap
                        (\index child ->
                            flattenTail (path ++ [ index ]) child
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

        Node _ children ->
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
                Dict.singleton current.path
                    { siblings = current.siblings
                    , children = List.indexedMap (\index _ -> current.path ++ [ index ]) children
                    }
                    :: List.map .nodeInfo childrenAnalysis
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
                            analysis.depth - List.length path - 1

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
                                            if n == depthDiff - 1 then
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
                        if List.member path analysis.shortNodes then
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
    in
    lowestLevelItems
        |> List.sortWith
            (\( path1, _ ) ( path2, _ ) ->
                Utils.compareLists path1 path2
            )
        |> List.indexedMap
            (\index ( id, _ ) ->
                ( id
                , { center =
                        ( toFloat index - toFloat (List.length lowestLevelItems - 1) / 2
                        , toFloat showLevels - 1
                        )
                  , childCenters = []
                  }
                )
            )
        |> Dict.fromList
        |> layoutLevelPass (showLevels - 2) nodeInfo


layoutLevelPass : Int -> NodeInfo -> Layout -> Layout
layoutLevelPass level nodeInfo currentLayoutPass =
    if level == -1 then
        currentLayoutPass

    else
        let
            properLevel =
                level
        in
        nodeInfo
            |> Dict.filter (\path _ -> List.length path == properLevel)
            |> Dict.toList
            |> List.map
                (\( path, { children } ) ->
                    ( path
                    , List.map
                        (\child ->
                            Dict.get child currentLayoutPass
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
                                        if List.length centers == 0 then
                                            0

                                        else
                                            List.foldl (+) 0 centers
                                                / (List.length centers |> toFloat)
                                in
                                { center =
                                    ( centerX
                                    , toFloat properLevel
                                    )
                                , childCenters = centers
                                }
                           )
                    )
                )
            |> Dict.fromList
            |> (\pass -> layoutLevelPass (properLevel - 1) nodeInfo (Dict.union currentLayoutPass pass))
