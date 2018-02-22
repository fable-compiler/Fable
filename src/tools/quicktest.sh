pushd ../dotnet/Fable.Compiler
# dotnet build --no-restore --no-dependencies
dotnet run yarn-splitter --cwd ../../tools --fable-core ../../../build/fable-core
popd
node temp/QuickTest.js