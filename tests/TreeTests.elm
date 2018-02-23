module TreeTests exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Arborist.Tree as Tree
import Utils.Tree


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
                    (Utils.Tree.delete [ 1, 0 ]
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
                Expect.equal (Utils.Tree.update [ 1, 0 ] "Apricot2" tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach"
                            [ Tree.Node "Apricot2" []
                            ]
                        ]
                    )
        , test "Inserts" <|
            \_ ->
                Expect.equal (Utils.Tree.insert [] (Just "Banana") tree)
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
        , test "Encodes and decoders" <|
            \_ ->
                Expect.equal (tree |> Tree.encoder Encode.string |> Decode.decodeValue (Tree.decoder Decode.string)) (Ok tree)
        ]
