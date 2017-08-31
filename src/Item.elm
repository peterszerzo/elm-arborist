module Item exposing (..)

import Json.Decode as Decode


type alias Item =
    { id : String
    , value : String
    }


decoder : Decode.Decoder Item
decoder =
    Decode.map2 Item
        (Decode.field "id" Decode.string)
        (Decode.field "value" Decode.string)
