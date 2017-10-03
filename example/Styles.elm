module Styles exposing (..)

{-| Program styling
-}


blue : String
blue =
    "#3E849B"


lightBlue : String
lightBlue =
    "#4594ad"


green : String
green =
    "#4DC433"


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
  font-family: monospace;
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
  font-size: 2rem;
  font-weight: 400;
}

h3 {
  font-size: 1.5rem;
  font-weight: 400;
}

h3:not(:first-child) {
  margin-top: 40px;
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
p {
  font-size: 1rem;
}

button {
  margin-top: 10px;
}
"""


box : List ( String, String )
box =
    [ ( "background-color", "white" )
    , ( "margin", "auto" )
    , ( "box-shadow", "0 0 12px rgba(0, 0, 0, 0.15)" )
    , ( "border-radius", "6px" )
    , ( "border", "1px solid " ++ faintGray )
    ]


popup : List ( String, String )
popup =
    [ ( "width", "360px" )
    , ( "background-color", "white" )
    , ( "height", "240px" )
    , ( "border-top", "4px solid #4DC433" )
    , ( "padding", "20px" )
    , ( "overflow", "auto" )
    , ( "position", "absolute" )
    , ( "transform", "translate3d(-50%, 75px, 0)" )
    , ( "box-shadow", "0 0 12px rgba(0, 0, 0, 0.25)" )
    ]


nodeContainer : List ( String, String )
nodeContainer =
    [ ( "width", "100%" )
    , ( "height", "60px" )
    , ( "border-radius", "4px" )
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
    , ( "line-height", "0.9" )
    , ( "text-align", "center" )
    ]


bubble : List ( String, String )
bubble =
    [ ( "position", "absolute" )
    , ( "box-sizing", "border-box" )
    , ( "border-radius", "6px" )
    , ( "border", "1px solid #3E849B" )
    , ( "width", "80px" )
    , ( "height", "30px" )
    , ( "padding-top", "6px" )
    , ( "color", "black" )
    , ( "text-align", "center" )
    , ( "background", "#FFFFFF" )
    , ( "top", "-58px" )
    , ( "left", "calc(50% - 40px + 2px)" )
    , ( "z-index", "200" )
    ]
