#!/bin/bash
# First time after cloning the repository, run:
#   bash quicktest.sh --build-library

# Next time, if src/fable-library files haven't changed, just:
#   bash quicktest.sh

ARGS="$@"

echo "dotnet SDK version"
dotnet --version

if [ "${ARGS/--build-js-library/}" != "$ARGS" -o "${ARGS/--build-library/}" != "$ARGS" ]; then
    echo "Building fable-library (JS/TS files)..."

    pushd ../..
    # npm i
    # npx tslint --project src/fable-library
    npx tsc --project src/fable-library
    popd
fi

if [ "${ARGS/--build-library/}" != "$ARGS" ]; then
    echo "Building fable-library (F# files)..."

    dotnet run -c Release -p ../Fable.Cli \
        fable-splitter --verbose --force-pkgs \
        --fable-library force:\${outDir} \
        --args "-c src/fable-library/splitter.config.js"
fi

dotnet run -p ../Fable.Cli \
    fable-splitter --force-pkgs \
    --args "-c src/quicktest/splitter.config.js $ARGS"

node temp/QuickTest.js