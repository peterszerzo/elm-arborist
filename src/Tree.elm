module Tree exposing (..)

import Json.Decode as Decode


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


decoder : Decode.Decoder a -> Decode.Decoder (Tree a)
decoder nodeDecoder =
    Decode.oneOf
        [ Decode.null Empty
        , Decode.map3 Node
            (Decode.field "value" nodeDecoder)
            (Decode.field "left" (Decode.lazy (\_ -> decoder nodeDecoder)))
            (Decode.field "right" (Decode.lazy (\_ -> decoder nodeDecoder)))
        ]



-- Update


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton item =
    Node item Empty Empty


map : (a -> b) -> Tree a -> Tree b
map fn tree =
    case tree of
        Empty ->
            Empty

        Node val left right ->
            Node (fn val) (map fn left) (map fn right)


find : (a -> Bool) -> Tree a -> List a
find fn tree =
    case tree of
        Empty ->
            []

        Node val left right ->
            (if fn val then
                [ val ]
             else
                []
            )
                ++ (find fn left)
                ++ (find fn right)


findOne : (a -> Bool) -> Tree a -> Maybe a
findOne fn tree =
    case tree of
        Empty ->
            Nothing

        Node val left right ->
            if fn val then
                Just val
            else
                case findOne fn left of
                    Just val ->
                        Just val

                    Nothing ->
                        findOne fn right


swapOne : (a -> Bool) -> (a -> Bool) -> Tree a -> Tree a
swapOne fn1 fn2 tree =
    let
        item1 =
            findOne fn1 tree

        item2 =
            findOne fn2 tree
    in
        case ( item1, item2 ) of
            ( Just item1, Just item2 ) ->
                map
                    (\item ->
                        if item == item1 then
                            item2
                        else if item == item2 then
                            item1
                        else
                            item
                    )
                    tree

            ( _, _ ) ->
                tree


insert : (a -> Bool) -> Bool -> Tree a -> Tree a -> Tree a
insert parentFindFn isLeft insertedTree tree =
    case tree of
        Empty ->
            tree

        Node item left right ->
            if parentFindFn item then
                (if isLeft then
                    Node item insertedTree right
                 else
                    Node item left insertedTree
                )
            else
                Node item (insert parentFindFn isLeft insertedTree left) (insert parentFindFn isLeft insertedTree right)
