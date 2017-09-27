module Views.Styles exposing (..)


coordinate : Float -> Float -> Float -> List ( String, String )
coordinate width x y =
    [ ( "left", "calc(50% + " ++ (toString (x - (width / 2))) ++ "px)" )
    , ( "top", "calc(10% + " ++ (toString y) ++ "px)" )
    ]


nodeBase : Float -> Float -> List ( String, String )
nodeBase width height =
    [ ( "width", (toString width) ++ "px" )
    , ( "height", "auto" )
    , ( "min-height", (toString height) ++ "px" )
    , ( "position", "absolute" )
    , ( "user-select", "none" )
    , ( "cursor", "pointer" )
    , ( "margin", "0" )
    ]


dragShadowNode : List ( String, String )
dragShadowNode =
    [ ( "background-color", "rgba(0, 0, 0, 0.05)" )
    ]
