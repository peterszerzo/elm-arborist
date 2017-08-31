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
  margin-top: 20px;
  display: block;
}

input,
label,
p {
  font-size: 1rem;
}
"""


box : List ( String, String )
box =
    [ ( "width", "560px" )
    , ( "height", "560px" )
    , ( "margin", "10px" )
    , ( "text-align", "left" )
    , ( "display", "inline-block" )
    , ( "vertical-align", "top" )
    , ( "position", "relative" )
    , ( "border-radius", "4px" )
    , ( "border", "1px solid " ++ faintGray )
    ]
