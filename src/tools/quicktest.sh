#!/bin/bash
# First time after cloning the repository, run:
#   bash quicktest.sh --build-library

# Next time, if src/js/fable-library files haven't changed, just:
#   bash quicktest.sh

ARGS="$@"

echo "dotnet SDK version"
dotnet --version

if [ "${ARGS/--build-js-library/}" != "$ARGS" -o "${ARGS/--build-library/}" != "$ARGS" ]; then
    echo "Building fable-library (JS/TS files)..."

    pushd ../..
    yarn
    # yarn tslint --project src/js/fable-library
    yarn tsc --project src/js/fable-library
    popd
fi

if [ "${ARGS/--build-library/}" != "$ARGS" ]; then
    echo "Building fable-library (F# files)..."

    dotnet run -c Release -p ../dotnet/Fable.Compiler \
        fable-splitter --force-pkgs \
        --fable-library force:\${outDir} \
        --args "-c src/js/fable-library/splitter.config.js"
fi

dotnet run -p ../dotnet/Fable.Compiler \
    fable-splitter --force-pkgs \
    --args "-c src/tools/splitter.config.js $ARGS"

node temp/QuickTest.js