rm -rf build
mkdir build
elm-make example/Main.elm --output=build/index.html
surge build elm-treditor.peterszerzo.com
