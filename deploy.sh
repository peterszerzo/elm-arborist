# Build
rm -rf build
mkdir build
cp examples/DemoWebsite/index.html build
cd examples
elm make DemoWebsite/Main.elm --output=../build/elm.js
uglifyjs ../build/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=../build/elm.js
cd ..

# Deploy
gh-pages -d build
