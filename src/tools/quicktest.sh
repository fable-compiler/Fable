pushd ../dotnet/Fable.Compiler
dotnet run $1 yarn-splitter --cwd ../../tools --fable-core ../../../build/fable-core
popd
node temp/QuickTest.js