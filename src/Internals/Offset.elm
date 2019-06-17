module Internals.Offset exposing (GetterSetterConfig, Offset, fromPt, noOffset, toPt)


type Offset
    = Offset Float Float


type alias GetterSetterConfig =
    { unitX : Float
    , unitY : Float
    }


noOffset : Offset
noOffset =
    Offset 0 0


toPt : GetterSetterConfig -> Offset -> ( Float, Float )
toPt config (Offset x y) =
    ( x * config.unitX
    , y * config.unitY
    )


fromPt : GetterSetterConfig -> ( Float, Float ) -> Offset
fromPt config ( x, y ) =
    Offset (x / config.unitX) (y / config.unitY)
