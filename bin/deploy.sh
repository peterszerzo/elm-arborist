rm -rf build
mkdir build
cd examples
elm-make Main.elm --output=../build/elm.js
cd ..
cp examples/index.html build
git branch -D gh-pages
git checkout --orphan gh-pages
cp build/* .
trash src examples tests
git add .
git commit -m "Deploy"
git push -f origin gh-pages
