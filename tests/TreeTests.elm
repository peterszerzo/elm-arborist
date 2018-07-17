module TreeTests exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Expect exposing (Expectation)
import Test exposing (..)
import Arborist.Tree as Tree
import Internal.Tree.Extra as TreeHelpers


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
                    (TreeHelpers.delete [ 1, 0 ]
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
                Expect.equal (TreeHelpers.updateAt [ 1, 0 ] "Apricot2" tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach"
                            [ Tree.Node "Apricot2" []
                            ]
                        ]
                    )
        , test "Updates with children" <|
            \_ ->
                Expect.equal (TreeHelpers.updateAtWithChildren [ 1, 0 ] "Apricot2" (Just [ "PeachChild1", "PeachChild2" ]) tree)
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
                Expect.equal (TreeHelpers.swap [ 0 ] [ 1 ] tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Peach"
                            [ Tree.Node "Apricot" []
                            ]
                        , Tree.Node "Pear" []
                        ]
                    )
        , test "Inserts" <|
            \_ ->
                Expect.equal (TreeHelpers.insert [] (Just "Banana") tree)
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
                    (TreeHelpers.addTrailingEmpties
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
                    (TreeHelpers.addTrailingEmptiesAdvanced
                        (\{ node, parent, siblings, children } ->
                            node /= "Apple" && parent == Just "Apple" && siblings == [ "Pear" ] && children == [ "Peach" ]
                        )
                        (Tree.Node "Apple"
                            [ Tree.Node "Pear" [ Tree.Node "Peach" [] ]
                            ]
                        )
                    )
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" [ Tree.Node "Peach" [], Tree.Empty ]
                        ]
                    )
        , test "Encodes and decoders" <|
            \_ ->
                Expect.equal (tree |> Tree.encoder Encode.string |> Decode.decodeValue (Tree.decoder Decode.string)) (Ok tree)
        ]
