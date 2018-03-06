module ExperimentalEditorApi.Node exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode


type alias Node =
    { condition : String
    , value : String
    , model : Maybe String
    }


placeholder : Node
placeholder =
    { condition = "Condition"
    , value = "Value"
    , model = Nothing
    }


encoder : Node -> Encode.Value
encoder node =
    Encode.object
        [ ( "condition", Encode.string node.condition )
        , ( "value", Encode.string node.value )
        , ( "model"
          , node.model
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        ]


decoder : Decode.Decoder Node
decoder =
    Decode.map3 Node
        (Decode.field "condition" Decode.string)
        (Decode.field "value" Decode.string)
        -- TODO: finish decoder
        (Decode.succeed Nothing)
