module Arborist.Config exposing (..)

import Html exposing (Html)


type alias Context item =
    { parent : Maybe item
    , siblings : List item
    , isActive : Bool
    , isDropTarget : Bool
    , position : ( Float, Float )
    }


type alias Config item msg =
    { view : Context item -> item -> Html msg
    , placeholderView : Context item -> Html msg
    , layout :
        { width : Float
        , height : Float
        , verticalGap : Float
        , horizontalGap : Float
        }
    }
