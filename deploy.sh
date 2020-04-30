# Build
cd website
PUBLIC_URL=./ elm-app build
cd ..
pwd

# Deploy
gh-pages -d website/build
