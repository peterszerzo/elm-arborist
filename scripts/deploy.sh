function deploy() {
  gh-pages build
}

function build() {
  rm -rf build
  mkdir -p build/Egg
  cp examples/DemoWebsite/index.html build

  cd examples
  elm make Simple/Main.elm --optimize --output=../build/elm.js
  cd ..
  uglifyjs build/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=build/elm.min.js

  # elm make DemoWebsite/Main.elm --output=../build/elm.js
  # cp DemoWebsite/index.html ../build
  # elm make Egg/Main.elm --output=../build/Egg/elm.js
  # cp Egg/index.html ../build/Egg
  # cp Egg/index.js ../build/Egg

  cd ..
}

build
# deploy
