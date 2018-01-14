pushd ../dotnet/dotnet-fable
dotnet build --no-restore
popd
dotnet ../dotnet/dotnet-fable/bin/Debug/netcoreapp2.0/dotnet-fable.dll yarn-rollup
node temp/QuickTest.js