#!/bin/bash
# First time after cloning the repository, run:
#   bash quicktest.sh --build-core

# Next time, if src/js/fable-core files haven't changed, just:
#   bash quicktest.sh

ARGS="$@"

echo "dotnet SDK version"
dotnet --version

if [ "${ARGS/--build-js-core/}" != "$ARGS" -o "${ARGS/--build-core/}" != "$ARGS" ]; then
    echo "Building fable-core (JS/TS files)..."

    pushd ../..
    yarn
    # yarn tslint --project src/js/fable-core
    yarn tsc --project src/js/fable-core
    popd
fi

if [ "${ARGS/--build-core/}" != "$ARGS" ]; then
    echo "Building fable-core (F# files)..."

    dotnet run -p ../dotnet/Fable.Compiler \
        fable-splitter --force-pkgs \
        --fable-core force:\${outDir} \
        --args "-c src/js/fable-core/splitter.config.js"
fi

dotnet run -p ../dotnet/Fable.Compiler \
    fable-splitter --force-pkgs \
    --args "-c src/tools/splitter.config.js $ARGS"
