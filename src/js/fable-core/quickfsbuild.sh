pushd ../../dotnet/Fable.Compiler
dotnet run $1 node-run ../fable-splitter/dist/cli \
    --cwd ../../js/fable-core --fable-core force:\${outDir} \
    --experimental overload-index
popd

# Check argument injections
pushd ../../tools/InjectProcessor
dotnet run
popd
