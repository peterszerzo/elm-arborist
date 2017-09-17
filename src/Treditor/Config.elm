module Treditor.Config exposing (..)


type alias NodeId =
    String


type alias Config item =
    { toId : item -> NodeId
    , view : item -> String
    , layout :
        { width : Float
        , height : Float
        , verticalGap : Float
        , horizontalGap : Float
        }
    }
