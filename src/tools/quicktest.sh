pushd ../dotnet/Fable.Compiler
dotnet build --no-restore --no-dependencies
popd
dotnet ../dotnet/Fable.Compiler/bin/Debug/netcoreapp2.0/Fable.Compiler.dll yarn-rollup
node temp/QuickTest.js