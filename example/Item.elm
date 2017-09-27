module Item exposing (..)

import Json.Decode as Decode


type alias Item =
    { question : String
    , answer : String
    }


init : Item
init =
    { question = "", answer = "" }


setQuestion : String -> Item -> Item
setQuestion val item =
    { item | question = val }


setAnswer : String -> Item -> Item
setAnswer val item =
    { item | answer = val }


decoder : Decode.Decoder Item
decoder =
    Decode.map2 Item
        (Decode.field "question" Decode.string)
        (Decode.field "answer" Decode.string)
