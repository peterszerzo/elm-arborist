rm -rf build
mkdir build
cd example
elm-make Main.elm --output=../build/elm.js
cp index.html ../build
surge ../build elm-arborist.peterszerzo.com
