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
    , ( "text-align", "center" )
    , ( "position", "absolute" )
    , ( "padding", "3px 0" )
    , ( "user-select", "none" )
    , ( "border-radius", "4px" )
    , ( "cursor", "pointer" )
    , ( "margin", "0" )
    ]


regularNode : List ( String, String )
regularNode =
    [ ( "background", "rgba(0, 0, 0, 0.8)" )
    , ( "color", "white" )
    ]


placeholderNode : List ( String, String )
placeholderNode =
    [ ( "border", "2px dashed #DDDDDD" )
    ]


highlightedPlaceholderNode : List ( String, String )
highlightedPlaceholderNode =
    [ ( "border", "2px dashed #333333" ) ]
