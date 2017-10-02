module Views.Styles exposing (..)

import Arborist.Config as Config


coordinate : Config.Layout -> ( Float, Float ) -> List ( String, String )
coordinate layout ( x, y ) =
    [ ( "left", ((toString << floor) (layout.canvasWidth / 2 + x - (layout.nodeWidth / 2))) ++ "px" )
    , ( "top", ((toString << floor) (layout.canvasHeight * 0.1 + y)) ++ "px" )
    ]


nodeBase : Config.Layout -> List ( String, String )
nodeBase layout =
    [ ( "width", (toString (floor layout.nodeWidth)) ++ "px" )
    , ( "height", "auto" )
    , ( "min-height", (toString (floor layout.nodeHeight)) ++ "px" )
    , ( "position", "absolute" )
    , ( "user-select", "none" )
    , ( "cursor", "pointer" )
    , ( "margin", "0" )
    ]


dragShadowNode : List ( String, String )
dragShadowNode =
    [ ( "background-color", "rgba(0, 0, 0, 0.05)" )
    ]
