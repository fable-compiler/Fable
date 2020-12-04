set -e
rm -rf build
dotnet fsi build.fsx test
dotnet fsi build.fsx test-js-fast
dotnet fsi build.fsx test-repos