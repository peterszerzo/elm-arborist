module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Data.Tree as Tree


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
                Expect.equal (Tree.delete [ 1, 0 ] tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach" []
                        ]
                    )
        , test "Updates" <|
            \_ ->
                Expect.equal (Tree.update [ 1, 0 ] "Apricot2" tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach"
                            [ Tree.Node "Apricot2" []
                            ]
                        ]
                    )
        , test "Inserts" <|
            \_ ->
                Expect.equal (Tree.insert [] "Banana" tree)
                    (Tree.Node "Apple"
                        [ Tree.Node "Pear" []
                        , Tree.Node "Peach"
                            [ Tree.Node "Apricot" []
                            ]
                        , Tree.Node "Banana" []
                        ]
                    )
        ]
