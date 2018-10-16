module TreeTests exposing (suite, tree)

import Arborist.Tree as Tree
import Expect exposing (Expectation)
import Internal.Tree.Extra as TreeExtra
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
                    (TreeExtra.delete [ 1, 0 ]
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
                Expect.equal (TreeExtra.updateAt [ 1, 0 ] "Apricot2" tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach"
                            [ Tree.Node "Apricot2" []
                            ]
                        ]
                    )
        , test "Updates with children" <|
            \_ ->
                Expect.equal (TreeExtra.updateAtWithChildren [ 1, 0 ] "Apricot2" (Just [ "PeachChild1", "PeachChild2" ]) tree)
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
                Expect.equal (TreeExtra.swap [ 0 ] [ 1 ] tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Peach"
                            [ Tree.Node "Apricot" []
                            ]
                        , Tree.Node "Pear" []
                        ]
                    )
        , test "Inserts" <|
            \_ ->
                Expect.equal (TreeExtra.insert [] (Just "Banana") tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach"
                            [ Tree.Node "Apricot" []
                            ]
                        , Tree.Node "Banana" []
                        ]
                    )
        , test "Calculates depth" <|
            \_ ->
                Expect.equal (Tree.depth tree) 3
        , test "Adds trailing empties" <|
            \_ ->
                Expect.equal
                    (TreeExtra.addTrailingEmpties
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
                    (TreeExtra.addTrailingEmptiesAdvanced
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
        , test "Layout" <|
            \_ ->
                Node
                    ""
                    [ Node "" []
                    , Node ""
                        [ Node "" []
                        ]
                    ]
                    |> TreeExtra.layout
                    |> Expect.equal (Dict.fromList [ ( [], { center = ( 0, 0 ), childCenters = [ 0, 0 ] } ), ( [ 0 ], { center = ( 0, 1 ), childCenters = [] } ), ( [ 1 ], { center = ( 0, 1 ), childCenters = [ 0 ] } ), ( [ 1, 0 ], { center = ( 0, 2 ), childCenters = [] } ) ])
        ]
