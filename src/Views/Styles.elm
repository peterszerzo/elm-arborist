module Views.Styles exposing (..)

import Arborist.Config as Config
import Utils


coordinate : Config.Layout -> ( Float, Float ) -> List ( String, String )
coordinate layout ( x, y ) =
    [ ( "left", Utils.floatToPxString (x - (layout.nodeWidth / 2)) )
    , ( "top", Utils.floatToPxString y )
    ]


nodeBase : Config.Layout -> List ( String, String )
nodeBase layout =
    [ ( "width", Utils.floatToPxString layout.nodeWidth )
    , ( "height", "auto" )
    , ( "min-height", Utils.floatToPxString layout.nodeHeight )
    , ( "position", "absolute" )
    , ( "user-select", "none" )
    , ( "cursor", "pointer" )
    , ( "margin", "0" )
    ]


dragShadowNode : List ( String, String )
dragShadowNode =
    [ ( "background-color", "rgba(0, 0, 0, 0.05)" )
    ]
