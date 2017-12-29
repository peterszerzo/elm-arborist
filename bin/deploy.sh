rm -rf build
mkdir build
cd example
elm-make Main.elm --output=../build/elm.js
cd ..
cp example/index.html build
git branch -D gh-pages
git checkout --orphan gh-pages
cp build/* .
trash src example tests
git add .
git commit -m "Deploy"
git push -f origin gh-pages
