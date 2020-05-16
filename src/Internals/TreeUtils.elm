module Internals.TreeUtils exposing
    ( Layout
    , NodeInfo
    , Path
    , TreeAnalysis
    , addTrailingEmpties
    , addTrailingEmptiesAdvanced
    , analyze
    , branchAt
    , clusterBy
    , delete
    , find
    , flatten
    , insert
    , layout
    , moveDown
    , moveLeft
    , moveRight
    , moveUp
    , removeEmpties
    , swap
    , swapBranchAt
    , updateBranchAt
    )

import Arborist.Tree exposing (..)
import Dict
import Internals.Utils as Utils



-- Node traversal


type alias Path =
    List Int


clusterBy : (node -> Bool) -> Tree node -> Tree node
clusterBy isNodeClustered tree =
    case tree of
        Node node children ->
            Node node
                (if isNodeClustered node then
                    []

                 else
                    List.map (clusterBy isNodeClustered) children
                )

        Empty ->
            Empty


{-| Using this method is necessary because `List.member` is not reliable with `elm make --optimize`
-}
member : Path -> List Path -> Bool
member current all =
    case all of
        [] ->
            False

        head :: tail ->
            Utils.areListsEqual current head || member current tail


confirm : List Path -> Path -> Maybe Path
confirm all current =
    current
        |> (\pth -> ( pth, member pth all ))
        |> (\( pth, isMember ) ->
                if isMember then
                    Just pth

                else
                    Nothing
           )


moveDown : List Path -> Maybe Path -> Maybe Path
moveDown all maybeCurrent =
    case maybeCurrent of
        Nothing ->
            Just []

        Just current ->
            let
                countChildren =
                    all
                        |> List.filter
                            (\iteratedPath ->
                                List.length iteratedPath
                                    == List.length current
                                    + 1
                                    && List.take (List.length iteratedPath - 1) iteratedPath
                                    == current
                            )
                        |> List.length
            in
            current
                ++ [ if modBy 2 countChildren == 0 then
                        countChildren // 2 - 1

                     else
                        countChildren // 2
                   ]
                |> confirm all
                |> Maybe.withDefault current
                |> Just


moveUp : List Path -> Maybe Path -> Maybe Path
moveUp all current =
    current
        |> Maybe.andThen ((\lst -> List.take (List.length lst - 1) lst) >> confirm all)
        |> Maybe.map Just
        |> Maybe.withDefault (Just [])


moveLeft : List Path -> Maybe Path -> Maybe Path
moveLeft all current =
    current
        |> Maybe.andThen
            (Utils.changeLastInList
                (\v ->
                    case v of
                        0 ->
                            case current of
                                Just pth ->
                                    all
                                        |> List.filter
                                            (\pth_ ->
                                                (List.length pth_ == List.length pth)
                                                    && (List.take (List.length pth_ - 1) pth_
                                                            == List.take (List.length pth - 1) pth
                                                       )
                                            )
                                        |> List.length
                                        |> (\v_ -> v_ - 1)

                                Nothing ->
                                    -1

                        v_ ->
                            v_ - 1
                )
                >> confirm all
            )
        |> Maybe.withDefault []
        |> Just


moveRight : List Path -> Maybe Path -> Maybe Path
moveRight all current =
    current
        |> Maybe.andThen
            (Utils.changeLastInList (\v -> v + 1)
                >> confirm all
            )
        |> Maybe.withDefault
            (Maybe.map (Utils.changeLastInList (\_ -> 0)) current
                |> Maybe.withDefault []
            )
        |> Just



-- Calculation types


type alias Layout =
    Dict.Dict Path
        { center : ( Float, Float )
        , childCenters : List Float
        }


type alias NodeInfo =
    Dict.Dict Path
        { siblings : Int
        , children : List Path
        }


type alias TreeAnalysis =
    { depth : Int
    , shortNodes : List Path
    , nodeInfo : NodeInfo
    }


{-| Add a placeholder element used to add new nodes to the tree
-}
addTrailingEmpties : Tree a -> Tree a
addTrailingEmpties =
    addTrailingEmptiesAdvanced (\_ -> True)


addTrailingEmptiesAdvanced :
    ({ node : a
     , parent : Maybe a
     , siblings : List a
     , children : List a
     }
     -> Bool
    )
    -> Tree a
    -> Tree a
addTrailingEmptiesAdvanced =
    addTrailingEmptiesAdvancedHelper { parent = Nothing, siblings = [] }


addTrailingEmptiesAdvancedHelper :
    { parent : Maybe a
    , siblings : List a
    }
    ->
        ({ node : a
         , parent : Maybe a
         , siblings : List a
         , children : List a
         }
         -> Bool
        )
    -> Tree a
    -> Tree a
addTrailingEmptiesAdvancedHelper context shouldAddEmpty tree =
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
                        shouldAddEmpty
                    )
                    children
                    ++ (if
                            shouldAddEmpty
                                { parent = context.parent
                                , siblings = context.siblings
                                , node = item
                                , children = childNodes
                                }
                        then
                            [ Empty
                            ]

                        else
                            []
                       )


removeEmpties : Tree a -> Tree a
removeEmpties tree =
    case tree of
        Empty ->
            Empty

        Node item children ->
            Node item
                (children
                    |> List.filter (\childTree -> childTree /= Empty)
                    |> List.map removeEmpties
                )


{-| Find item by path
-}
find : Path -> Tree a -> Maybe (Tree a)
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


swap : Path -> Path -> Tree a -> Tree a
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
                |> swapBranchAt path1 st2
                |> swapBranchAt path2 st1
        )
        subtree1
        subtree2
        |> Maybe.withDefault tree


swapBranchAt : Path -> Tree a -> Tree a -> Tree a
swapBranchAt path subtree tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            Node item <|
                List.indexedMap
                    (\index child ->
                        if index == head then
                            swapBranchAt tail subtree child

                        else
                            child
                    )
                    children

        ( [], _ ) ->
            subtree

        ( _, currentTree ) ->
            currentTree


branchAt : Path -> Tree a -> Maybe (Tree a)
branchAt path tree =
    case ( path, tree ) of
        ( head :: tail, Node _ children ) ->
            children
                |> List.indexedMap
                    (\index child ->
                        if index == head then
                            Just child

                        else
                            Nothing
                    )
                |> List.filterMap identity
                |> List.head
                |> Maybe.andThen (branchAt tail)

        ( [], branch ) ->
            Just branch

        ( _, anyTree ) ->
            Just anyTree


updateBranchAt : Path -> (Tree a -> Tree a) -> Tree a -> Tree a
updateBranchAt path updateBranchModifier tree =
    case ( path, tree ) of
        ( head :: tail, Node item children ) ->
            Node item <|
                List.indexedMap
                    (\index child ->
                        if index == head then
                            updateBranchAt tail updateBranchModifier child

                        else
                            child
                    )
                    children

        ( [], branch ) ->
            updateBranchModifier branch

        ( _, anyTree ) ->
            anyTree


insert : Path -> Tree a -> Tree a -> Tree a
insert path insertItem tree =
    case ( path, tree ) of
        ( _ :: _, Empty ) ->
            insertItem

        ( head :: tail, Node item children ) ->
            children
                |> List.indexedMap
                    (\index child ->
                        if index == head then
                            insert tail insertItem child

                        else
                            child
                    )
                |> (\currentChildren ->
                        currentChildren
                            ++ (if head == List.length currentChildren && tail == [] then
                                    [ insertItem
                                    ]

                                else
                                    []
                               )
                   )
                |> Node item

        ( [], _ ) ->
            insertItem


delete : Path -> Tree a -> Tree a
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
flatten : Tree a -> List ( Path, Maybe a )
flatten =
    flattenTail []


flattenTail : Path -> Tree a -> List ( Path, Maybe a )
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


layout : Tree a -> Layout
layout tree =
    let
        analysis =
            analyze tree

        shortNodes =
            analysis.shortNodes

        dirtyLayout =
            dirtyAnalysisToLayout analysis
    in
    dirtyLayout
        |> Dict.map
            (\path { center, childCenters } ->
                { center = center
                , childCenters =
                    if List.member path shortNodes then
                        []

                    else
                        childCenters
                }
            )
        |> normalizeToRoot


normalizeToRoot : Layout -> Layout
normalizeToRoot treeLayout =
    let
        ( rootCenterX, rootCenterY ) =
            treeLayout
                |> Utils.dictGetWithListKeys []
                |> Maybe.map .center
                |> Maybe.withDefault ( 0, 0 )
    in
    Dict.map
        (\_ treeLayoutItem ->
            { center =
                treeLayoutItem.center
                    |> (\( x, y ) -> ( x - rootCenterX, y - rootCenterY ))
            , childCenters = List.map (\center -> center - rootCenterX) treeLayoutItem.childCenters
            }
        )
        treeLayout


{-| Analyze tree.
-}
analyze : Tree a -> TreeAnalysis
analyze tree =
    analyzeTail
        { depth = depth tree
        , current = { path = [], siblings = 0 }
        }
        tree


analyzeTail : { depth : Int, current : { path : Path, siblings : Int } } -> Tree a -> TreeAnalysis
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
            , shortNodes =
                (if List.length children == 0 && List.length current.path < depth - 1 then
                    [ current.path ]

                 else
                    []
                )
                    ++ (childrenAnalysis
                            |> List.map .shortNodes
                            |> List.foldl (++) []
                       )
            }


{-| Lays out elements. The dirty prefix refers to the fact that this algorithm leaves in data related to filler nodes, cleaned up in the main `layout` method
-}
dirtyAnalysisToLayout : TreeAnalysis -> Layout
dirtyAnalysisToLayout analysis =
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
                |> List.map
                    (\{ path, siblings, children } ->
                        ( path
                        , { siblings = siblings, children = children }
                        )
                    )
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
        |> dirtyAnalysisToLayoutLevelPass (showLevels - 2) nodeInfo


dirtyAnalysisToLayoutLevelPass : Int -> NodeInfo -> Layout -> Layout
dirtyAnalysisToLayoutLevelPass level nodeInfo currentLayoutPass =
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
                            Utils.dictGetWithListKeys child currentLayoutPass
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
            |> (\pass ->
                    dirtyAnalysisToLayoutLevelPass
                        (properLevel - 1)
                        nodeInfo
                        (Dict.union currentLayoutPass pass)
               )
