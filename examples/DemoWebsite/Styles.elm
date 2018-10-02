module DemoWebsite.Styles exposing
    ( black
    , blue
    , box
    , button
    , faintGray
    , gray
    , green
    , lightBlue
    , nodeContainer
    , orange
    , popup
    , raw
    , text
    )

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (style)


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


box : List (Attribute msg)
box =
    [ style "margin" "auto"
    ]


popup : List (Attribute msg)
popup =
    [ style "width" "320px"
    , style "background-color" "white"
    , style "height" "240px"
    , style "border-top" <| "4px solid " ++ green
    , style "padding" "20px 20px 0 20px"
    , style "border-radius" "4px"
    , style "overflow" "auto"
    , style "position" "absolute"
    , style "transform" "translate3d(-50%, 50px, 0)"
    , style "box-shadow" "0 2px 4px rgba(0, 0, 0, 0.15)"
    ]


nodeContainer : List (Attribute msg)
nodeContainer =
    [ style "width" "100%"
    , style "height" "45px"
    , style "font-size" "0.875rem"
    , style "padding" "4px 20px"
    , style "border-radius" "4px"
    , style "box-sizing" "border-box"
    , style "padding" "0 12px"
    , style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    ]


text : List (Attribute msg)
text =
    [ style "margin" "0"
    , style "font-size" "0.875rem"
    , style "line-height" "1.2"
    , style "text-align" "center"
    ]


button : List (Attribute msg)
button =
    [ style "background-color" green
    , style "border" "0"
    , style "padding" "6px 12px"
    , style "margin" "20px auto"
    , style "color" "#FFFFFF"
    ]
