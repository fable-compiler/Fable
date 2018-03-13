pushd ../../dotnet/Fable.Compiler
dotnet run $1 node-run ../fable-splitter/dist/cli \
    --cwd ../../js/fable-core --fable-core force:\${outDir} \
    --args "-c splitter.config.js"
popd
