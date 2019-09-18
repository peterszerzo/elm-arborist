module TreeTests exposing (suite, tree)

import Arborist.Tree as Tree
import Dict
import Expect exposing (Expectation)
import Internals.TreeUtils as TreeUtils
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


tree : Tree.Tree String
tree =
    Tree.Node "Apple"
        [ Tree.Node "Pear" []
        , Tree.Node "Peach"
            [ Tree.Node "Apricot" []
            ]
        ]


suite : Test
suite =
    describe "Tree"
        [ test "Deletes" <|
            \_ ->
                Expect.equal
                    (TreeUtils.delete [ 1, 0 ]
                        (Tree.Node "Apple"
                            [ Tree.Node "Pear"
                                [ Tree.Node "Pear2" []
                                ]
                            , Tree.Node "Peach"
                                [ Tree.Node "Apricot" []
                                ]
                            ]
                        )
                    )
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" [ Tree.Node "Pear2" [] ]
                        , Tree.Node "Peach" []
                        ]
                    )
        , test "Updates" <|
            \_ ->
                Expect.equal
                    (TreeUtils.updateAt
                        [ 1, 0 ]
                        (\_ -> Tree.Node "Apricot2" [])
                        tree
                    )
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach"
                            [ Tree.Node "Apricot2" []
                            ]
                        ]
                    )
        , test "Updates with children" <|
            \_ ->
                Expect.equal
                    (TreeUtils.updateAt
                        [ 1, 0 ]
                        (\_ ->
                            Tree.Node "Apricot2"
                                [ Tree.Node "PeachChild1" []
                                , Tree.Node "PeachChild2" []
                                ]
                        )
                        tree
                    )
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach"
                            [ Tree.Node "Apricot2"
                                [ Tree.Node "PeachChild1" []
                                , Tree.Node "PeachChild2" []
                                ]
                            ]
                        ]
                    )
        , test "Swaps" <|
            \_ ->
                Expect.equal (TreeUtils.swap [ 0 ] [ 1 ] tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Peach"
                            [ Tree.Node "Apricot" []
                            ]
                        , Tree.Node "Pear" []
                        ]
                    )
        , test "Inserts" <|
            \_ ->
                Expect.equal
                    (TreeUtils.insert
                        []
                        (Tree.Node "Banana" [])
                        tree
                    )
                    (Tree.Node "Banana" [])
        , test "Calculates depth" <|
            \_ ->
                Expect.equal (Tree.depth tree) 3
        , test "Adds trailing empties" <|
            \_ ->
                Expect.equal
                    (TreeUtils.addTrailingEmpties
                        (Tree.Node "Apple"
                            [ Tree.Node "Pear" []
                            ]
                        )
                    )
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" [ Tree.Empty ]
                        , Tree.Empty
                        ]
                    )
        , test "Adds trailing conditionally" <|
            \_ ->
                Expect.equal
                    (TreeUtils.addTrailingEmptiesAdvanced
                        (\{ node, parent, siblings, children } ->
                            node
                                == "Pear"
                                && (List.length children == 1)
                        )
                        (Tree.Node "Apple"
                            [ Tree.Node "Pear" [ Tree.Node "Peach" [] ]
                            , Tree.Node "Pear2"
                                [ Tree.Node "Peach2"
                                    [ Tree.Node "Pear" [ Tree.Node "Peach" [] ]
                                    ]
                                ]
                            ]
                        )
                    )
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" [ Tree.Node "Peach" [], Tree.Empty ]
                        , Tree.Node "Pear2"
                            [ Tree.Node "Peach2"
                                [ Tree.Node "Pear"
                                    [ Tree.Node "Peach" [], Tree.Empty ]
                                ]
                            ]
                        ]
                    )
        , test "Encodes and decoders" <|
            \_ ->
                Expect.equal (tree |> Tree.encoder Encode.string |> Decode.decodeValue (Tree.decoder Decode.string)) (Ok tree)
        , test "Simple layout" <|
            \_ ->
                Tree.Node ""
                    [ Tree.Node "" []
                    , Tree.Node "" []
                    ]
                    |> TreeUtils.layout
                    |> Expect.equal
                        (Dict.fromList
                            [ ( [], { center = ( 0, 0 ), childCenters = [ -0.5, 0.5 ] } )
                            , ( [ 0 ], { center = ( -0.5, 1 ), childCenters = [] } )
                            , ( [ 1 ], { center = ( 0.5, 1 ), childCenters = [] } )
                            ]
                        )
        , test "Tree analysis" <|
            \_ ->
                Tree.Node ""
                    [ Tree.Node "" []
                    , Tree.Node ""
                        [ Tree.Node "" []
                        ]
                    ]
                    |> TreeUtils.analyze
                    |> Expect.equal
                        { depth = 3
                        , shortNodes = [ [ 0 ] ]
                        , nodeInfo =
                            Dict.fromList
                                [ ( []
                                  , { children = [ [ 0 ], [ 1 ] ]
                                    , siblings = 0
                                    }
                                  )
                                , ( [ 0 ]
                                  , { children = []
                                    , siblings = 1
                                    }
                                  )
                                , ( [ 1 ]
                                  , { children = [ [ 1, 0 ] ]
                                    , siblings = 1
                                    }
                                  )
                                , ( [ 1, 0 ]
                                  , { children = []
                                    , siblings = 0
                                    }
                                  )
                                ]
                        }
        , test "Trivial layout" <|
            \_ ->
                Tree.Node ""
                    []
                    |> TreeUtils.layout
                    |> Expect.equal
                        (Dict.fromList
                            [ ( [], { center = ( 0, 0 ), childCenters = [] } )
                            ]
                        )
        , test "Complex layout" <|
            \_ ->
                Tree.Node ""
                    [ Tree.Node "" []
                    , Tree.Node ""
                        [ Tree.Node "" []
                        ]
                    ]
                    |> TreeUtils.layout
                    |> Expect.equal
                        (Dict.fromList
                            [ ( [], { center = ( 0, 0 ), childCenters = [ -0.5, 0.5 ] } )
                            , ( [ 0 ], { center = ( -0.5, 1 ), childCenters = [] } )

                            -- This is a filler node
                            , ( [ 0, 0 ], { center = ( -0.5, 2 ), childCenters = [] } )
                            , ( [ 1 ], { center = ( 0.5, 1 ), childCenters = [ 0.5 ] } )
                            , ( [ 1, 0 ], { center = ( 0.5, 2 ), childCenters = [] } )
                            ]
                        )
        ]
