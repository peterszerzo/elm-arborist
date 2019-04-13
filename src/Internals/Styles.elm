module Internals.Styles exposing (coordinate, nodeBase)

import Internals.Settings as Settings
import Internals.Utils as Utils


coordinate : Settings.Settings node -> ( Float, Float ) -> List ( String, String )
coordinate settings ( x, y ) =
    [ ( "left", Utils.floatToPxString (x - (settings.nodeWidth / 2)) )
    , ( "top", Utils.floatToPxString y )
    ]


nodeBase : Settings.Settings node -> List ( String, String )
nodeBase settings =
    [ ( "width", Utils.floatToPxString settings.nodeWidth )
    , ( "height", Utils.floatToPxString settings.nodeHeight )
    , ( "position", "absolute" )
    , ( "margin", "0" )
    ]
