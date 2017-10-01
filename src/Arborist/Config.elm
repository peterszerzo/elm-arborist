module Arborist.Config exposing (..)

import Html exposing (Html)


type NodeState
    = Normal
    | Active
    | DropTarget


type alias Context item =
    { parent : Maybe item
    , siblings : List item
    , state : NodeState
    , position : ( Float, Float )
    }


type alias Config item msg =
    { view : Context item -> Maybe item -> Html msg
    , layout :
        { width : Float
        , height : Float
        , level : Float
        , gutter : Float
        }
    }
