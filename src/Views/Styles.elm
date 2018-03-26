module Views.Styles exposing (..)

import Internal.Settings as Settings
import Utils


coordinate : Settings.Settings -> ( Float, Float ) -> List ( String, String )
coordinate settings ( x, y ) =
    [ ( "left", Utils.floatToPxString (x - (settings.nodeWidth / 2)) )
    , ( "top", Utils.floatToPxString y )
    ]


nodeBase : Settings.Settings -> List ( String, String )
nodeBase settings =
    [ ( "width", Utils.floatToPxString settings.nodeWidth )
    , ( "height", "auto" )
    , ( "min-height", Utils.floatToPxString settings.nodeHeight )
    , ( "position", "absolute" )
    , ( "user-select", "none" )
    , ( "cursor", "pointer" )
    , ( "margin", "0" )
    ]


dragShadowNode : List ( String, String )
dragShadowNode =
    [ ( "background-color", "rgba(0, 0, 0, 0.05)" )
    ]
