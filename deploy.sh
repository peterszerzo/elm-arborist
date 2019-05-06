# Build
cd landing
elm-app build
cd ..
pwd

# Deploy
gh-pages -d landing/build
