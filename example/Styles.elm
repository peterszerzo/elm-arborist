module Styles exposing (..)

{-| Program styling
-}


blue : String
blue =
    "#075D84"


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

body {
  background-color: #D3DBDC;
  padding: 30px;
}

h1 {
  margin: 0;
}

p {
  margin: 6 0 24px 0;
}

.intro {
  text-align: center;
  position: absolute;
  bottom: 20px;
  left: 20px;
  z-index: 100;
  padding: 20px;
  box-shadow: 0 0 12px rgba(0, 0, 0, 0.25);
  border-radius: 4px;
}

.intro__icon {
  font-size: 2.5rem;
  line-height: 1;
  position: absolute;
  top: -65px;
  left: calc(50% - 20px);
}

a {
  text-decoration: none;
  color: inherit;
  border-bottom: 1px solid currentColor;
}

input {
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

input:focus {
  border-color: """ ++ blue ++ """;
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
a {
  font-size: 0.75rem;
  line-height: 1.6;
}

button {
  margin-top: 10px;
}
"""


box : List ( String, String )
box =
    [ ( "background-color", "white" )
    , ( "margin", "auto" )
    ]


popup : List ( String, String )
popup =
    [ ( "width", "320px" )
    , ( "background-color", "white" )
    , ( "height", "220px" )
    , ( "border-top", "4px solid " ++ green )
    , ( "padding", "20px 20px 0 20px" )
    , ( "overflow", "auto" )
    , ( "position", "absolute" )
    , ( "transform", "translate3d(-50%, 50px, 0)" )
    , ( "box-shadow", "0 0 12px rgba(0, 0, 0, 0.25)" )
    ]


nodeContainer : List ( String, String )
nodeContainer =
    [ ( "width", "100%" )
    , ( "height", "36px" )
    , ( "border-radius", "4px" )
    , ( "font-size", "0.5rem" )
    , ( "padding", "4px 20px" )
    , ( "box-sizing", "border-box" )
    , ( "padding", "5px" )
    , ( "display", "flex" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    ]


text : List ( String, String )
text =
    [ ( "margin", "0" )
    , ( "font-size", "0.75" )
    , ( "line-height", "1.2" )
    , ( "text-align", "center" )
    ]


button : List ( String, String )
button =
    [ ( "background-color", green )
    , ( "border", "0" )
    , ( "border-radius", "4px" )
    , ( "padding", "6px 12px" )
    , ( "margin", "20px auto" )
    , ( "color", "#FFFFFF" )
    ]


bubble : List ( String, String )
bubble =
    [ ( "position", "absolute" )
    , ( "box-sizing", "border-box" )
    , ( "border-radius", "4px" )
    , ( "border", "1px solid #3E849B" )
    , ( "width", "40px" )
    , ( "height", "24px" )
    , ( "font-size", "0.75rem" )
    , ( "padding-top", "3px" )
    , ( "color", "black" )
    , ( "text-align", "center" )
    , ( "background", "#FFFFFF" )
    , ( "top", "-44px" )
    , ( "left", "calc(50% - 20px)" )
    , ( "z-index", "200" )
    ]
