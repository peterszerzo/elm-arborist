module Simple.Styles exposing (black, blue, bodyText, box, bubble, button, faintGray, gray, green, lightBlue, nodeContainer, orange, popup, raw)

import Html exposing (Html, p, text)
import Html.Attributes exposing (style)


{-| Program styling
-}
blue : String
blue =
    "#046896"


lightBlue : String
lightBlue =
    "#347A9A"


green : String
green =
    "#037C4E"


orange : String
orange =
    "#F18F01"


gray : String
gray =
    "#828481"


faintGray : String
faintGray =
    "#D1D7D4"


black : String
black =
    "#090B09"


raw : String
raw =
    """
* {
  -webkit-font-smoothing: antialiased;
  box-sizing: border-box;
  font-family: 'PT Sans', sans-serif;
}

h1 {
  margin: 0;
}

p {
  margin: 6px 0 0 0;
}

input, textarea {
  outline: none;
  display: block;
  width: 100%;
  padding: 4px 8px;
  margin-top: 4px;
  border-radius: 4px;
  border: 1px solid """ ++ faintGray ++ """;
  transition: border-color 0.3s;
}

h1 {
  font-size: 1.5rem;
  font-weight: 400;
  margin: 0.5rem;
}

h2 {
  font-size: 0.75rem;
  font-weight: 400;
  margin: 0.5rem;
}

input:focus,
textarea:focus {
  box-shadow: 0 0 3px """ ++ green ++ """;
  border-color: """ ++ green ++ """;
}

label {
  color: """ ++ gray ++ """;
  display: block;
}

label:not(:first-of-type) {
  margin-top: 15px;
}

input,
label,
p,
button,
a {
  font-size: 0.875rem;
}

input,
label,
p {
  line-height: 1.6;
}

button {
  margin-top: 10px;
}

button:focus {
  outline: none;
  box-shadow: 0 0 3px """ ++ green ++ """;
}
"""


box : List ( String, String )
box =
    [ ( "margin", "auto" )
    , ( "border", "1px solid #adadad" )
    ]


popup : List ( String, String )
popup =
    [ ( "width", "320px" )
    , ( "background-color", "white" )
    , ( "height", "240px" )
    , ( "border-top", "4px solid " ++ green )
    , ( "padding", "20px 20px 0 20px" )
    , ( "border-radius", "4px" )
    , ( "overflow", "auto" )
    , ( "position", "absolute" )
    , ( "transform", "translate3d(-50%, 50px, 0)" )
    , ( "box-shadow", "0 2px 4px rgba(0, 0, 0, 0.15)" )
    ]


nodeContainer : List ( String, String )
nodeContainer =
    [ ( "width", "100%" )
    , ( "height", "45px" )
    , ( "font-size", "0.875rem" )
    , ( "padding", "4px 20px" )
    , ( "border-radius", "4px" )
    , ( "box-sizing", "border-box" )
    , ( "padding", "0 12px" )
    , ( "display", "flex" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    ]


bodyText : String -> Html msg
bodyText body =
    p
        [ style "margin" "0"
        , style "font-size" "0.875rem"
        , style "line-height" "1.2"
        , style "text-align" "center"
        ]
        [ text body ]


bubble : String -> Html msg
bubble body =
    p
        [ style "position" "absolute"
        , style "box-sizing" "border-box"
        , style "width" "fit-content"
        , style "min-width" "48px"
        , style "height" "28px"
        , style "border-radius" "14px"
        , style "border" "2px solid #E2E2E2"
        , style "background-color" "#FFF"
        , style "font-size" "0.75rem"
        , style "padding" "0 12px"
        , style "color" "black"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "top" "-46px"
        , style "left" "50%"
        , style "transform" "translateX(-50%)"
        , style "z-index" "200"
        ]
        [ text body ]


button : List ( String, String )
button =
    [ ( "background-color", green )
    , ( "border", "0" )
    , ( "padding", "6px 12px" )
    , ( "margin", "20px auto" )
    , ( "color", "#FFFFFF" )
    ]
