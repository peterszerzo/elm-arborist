module Internals.Pt exposing (Pt, add, scale)


type alias Pt =
    ( Float, Float )


add : Pt -> Pt -> Pt
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


scale : Float -> Pt -> Pt
scale factor ( x, y ) =
    ( x * factor, y * factor )
