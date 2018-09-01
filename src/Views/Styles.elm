module Views.Styles exposing (coordinate, nodeBase)

import Internal.Settings as Settings
import Utils


coordinate : Settings.Settings node -> ( Float, Float ) -> List ( String, String )
coordinate settings ( x, y ) =
    [ ( "left", Utils.floatToPxString (x - (settings.nodeWidth / 2)) )
    , ( "top", Utils.floatToPxString y )
    ]


nodeBase : Settings.Settings node -> List ( String, String )
nodeBase settings =
    [ ( "width", Utils.floatToPxString settings.nodeWidth )
    , ( "height", "auto" )
    , ( "min-height", Utils.floatToPxString settings.nodeHeight )
    , ( "position", "absolute" )
    , ( "user-select", "none" )
    , ( "cursor", "pointer" )
    , ( "margin", "0" )
    ]
