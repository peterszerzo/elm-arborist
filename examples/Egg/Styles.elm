module Egg.Styles exposing (black, blue, box, button, faintGray, gray, green, lightBlue, nodeContainer, orange, popup, raw, text)

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
    "#197753"


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

a {
  text-decoration: none;
  color: inherit;
  border-bottom: 1px solid currentColor;
  line-height: 1;
}

a:hover {
  color: """ ++ green ++ """;
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
    ]


popup : List ( String, String )
popup =
    [ ( "width", "320px" )
    , ( "background-color", "white" )
    , ( "height", "240px" )
    , ( "border-top", "4px solid " ++ green )
    , ( "padding", "20px 20px 0 20px" )
    , ( "overflow", "auto" )
    , ( "position", "absolute" )
    , ( "transform", "translate3d(-50%, 50px, 0)" )
    , ( "box-shadow", "0 0 12px rgba(0, 0, 0, 0.2)" )
    ]


nodeContainer : List ( String, String )
nodeContainer =
    [ ( "width", "100%" )
    , ( "height", "45px" )
    , ( "font-size", "0.875rem" )
    , ( "padding", "4px 20px" )
    , ( "box-sizing", "border-box" )
    , ( "padding", "0 12px" )
    , ( "display", "flex" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    ]


text : List ( String, String )
text =
    [ ( "margin", "0" )
    , ( "font-size", "0.875rem" )
    , ( "line-height", "1.2" )
    , ( "text-align", "center" )
    ]


button : List ( String, String )
button =
    [ ( "background-color", green )
    , ( "border", "0" )
    , ( "padding", "6px 12px" )
    , ( "margin", "20px auto" )
    , ( "color", "#FFFFFF" )
    ]
