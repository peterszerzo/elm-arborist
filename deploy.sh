# Build
cd landing
PUBLIC_URL=./ elm-app build
cd ..
pwd

# Deploy
gh-pages -d landing/build
