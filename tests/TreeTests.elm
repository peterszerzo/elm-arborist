module TreeTests exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Expect exposing (Expectation)
import Test exposing (..)
import Arborist
import Tree
import Tree.Extra as TreeHelpers


tree : Tree.Tree String
tree =
    Arborist.node "Apple"
        [ Arborist.node "Pear" []
        , Arborist.node "Peach"
            [ Arborist.node "Apricot" []
            ]
        ]


suite : Test
suite =
    describe "Tree"
        [ test "Deletes" <|
            \_ ->
                Expect.equal
                    (TreeHelpers.delete [ 1, 0 ]
                        (Arborist.node "Apple"
                            [ Arborist.node "Pear"
                                [ Arborist.node "Pear2" []
                                ]
                            , Arborist.node "Peach"
                                [ Arborist.node "Apricot" []
                                ]
                            ]
                        )
                    )
                    (Arborist.node "Apple"
                        [ Arborist.node "Pear" [ Arborist.node "Pear2" [] ]
                        , Arborist.node "Peach" []
                        ]
                    )
        , test "Updates" <|
            \_ ->
                Expect.equal (TreeHelpers.update [ 1, 0 ] "Apricot2" tree)
                    (Arborist.node "Apple"
                        [ Arborist.node "Pear" []
                        , Arborist.node "Peach"
                            [ Arborist.node "Apricot2" []
                            ]
                        ]
                    )
        , test "Inserts" <|
            \_ ->
                Expect.equal (TreeHelpers.insert [] (Just "Banana") tree)
                    (Arborist.node "Apple"
                        [ Arborist.node "Pear" []
                        , Arborist.node "Peach"
                            [ Arborist.node "Apricot" []
                            ]
                        , Arborist.node "Banana" []
                        ]
                    )
        , test "Calculates depth" <|
            \_ ->
                Expect.equal (Tree.depth tree) 3
        , test "Encodes and decoders" <|
            \_ ->
                Expect.equal (tree |> Tree.encoder Encode.string |> Decode.decodeValue (Tree.decoder Decode.string)) (Ok tree)
        ]
