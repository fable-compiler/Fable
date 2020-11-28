set -e
rm -rf build
npm test
npm run build test-js-fast
npm run build test-repos