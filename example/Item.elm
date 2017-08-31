module Item exposing (..)

import Json.Decode as Decode


type alias Item =
    { id : String
    , value : String
    }


init : Item
init =
    { id = "1234"
    , value = ""
    }


setId : String -> Item -> Item
setId newId item =
    { item | id = newId }


setValue : String -> Item -> Item
setValue newValue item =
    { item | value = newValue }


decoder : Decode.Decoder Item
decoder =
    Decode.map2 Item
        (Decode.field "id" Decode.string)
        (Decode.field "value" Decode.string)
