module Landing.Node exposing (Node, setAnswer, setQuestion)


type alias Node =
    { question : String
    , answer : String
    , isClustered : Bool
    }


setQuestion : String -> Node -> Node
setQuestion val item =
    { item | question = val }


setAnswer : String -> Node -> Node
setAnswer val item =
    { item | answer = val }
