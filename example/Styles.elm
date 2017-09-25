module Styles exposing (..)

import Colors exposing (..)


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
  padding: 40px;
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
    [ ( "width", "1000px" )
    , ( "height", "560px" )
    , ( "background-color", "white" )
    , ( "margin", "auto" )
    , ( "text-align", "left" )
    , ( "vertical-align", "top" )
    , ( "overflow", "hidden" )
    , ( "position", "relative" )
    , ( "box-shadow", "0 0 12px rgba(0, 0, 0, 0.15)" )
    , ( "border-radius", "6px" )
    , ( "border", "1px solid " ++ faintGray )
    ]


popup : List ( String, String )
popup =
    [ ( "width", "240px" )
    , ( "background-color", "white" )
    , ( "height", "180px" )
    , ( "padding", "20px" )
    , ( "overflow", "auto" )
    , ( "position", "absolute" )
    , ( "bottom", "20px" )
    , ( "box-shadow", "0 0 6px rgba(0, 0, 0, 0.2)" )
    , ( "right", "20px" )
    ]
