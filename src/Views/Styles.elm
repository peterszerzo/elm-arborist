module Views.Styles exposing (..)

import Time
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


throttleTransitionStyles : List String -> Maybe Time.Time -> List ( String, String )
throttleTransitionStyles styleProperties throttle =
    -- TODO: this is currently disabled for lack of reliability.
    if True then
        []
    else
        case throttle of
            Nothing ->
                []

            Just time ->
                [ ( "transition"
                  , styleProperties
                        |> List.map (\property -> property ++ " " ++ (time / 1000 |> toString) ++ "s linear")
                        |> String.join ", "
                  )
                ]
