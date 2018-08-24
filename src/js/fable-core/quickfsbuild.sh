dotnet run -p ../../dotnet/Fable.Compiler \
    node-run ../fable-splitter/dist/cli --fable-core force:\${outDir}

# Check argument injections
dotnet run -p ../../tools/InjectProcessor
