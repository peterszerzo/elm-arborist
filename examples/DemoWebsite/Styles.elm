module DemoWebsite.Styles exposing (..)

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

.intro {
  text-align: center;
  position: absolute;
  bottom: 20px;
  left: 20px;
  z-index: 100;
  padding: 20px;
  border-radius: 4px;
  background-color: #fafafa;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.intro a {
  display: inline-block;
  margin: 0 8px;
}

.intro__icon {
  width: 60px;
  height: 60px;
  margin: -10px auto;
}

.intro__icon svg {
  width: 100%;
  height: 100%;
  fill: """ ++ green ++ """;
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


button : List ( String, String )
button =
    [ ( "background-color", green )
    , ( "border", "0" )
    , ( "padding", "6px 12px" )
    , ( "margin", "20px auto" )
    , ( "color", "#FFFFFF" )
    ]
