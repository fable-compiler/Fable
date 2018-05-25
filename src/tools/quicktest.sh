#!/bin/bash
# First time after cloning the repository, run:
#   bash quicktest.sh --build-core

# Next time, if src/js/fable-core files haven't changed, just:
#   bash quicktest.sh

ARGS="$@"

echo "dotnet SDK version"
dotnet --version

dotnet build ../dotnet/Fable.Compiler

if [ "${ARGS/--build-core/}" != "$ARGS" ]; then
    echo "Building fable-core..."
    rm -rf .fable

    pushd ../..
    dotnet publish src/dotnet/Fable.Core -o ../../../build/fable
    yarn

    # Compile fable-splitter
    yarn tsc --project src/js/fable-splitter
    cp src/js/fable-splitter/src/*.js src/js/fable-splitter/dist/

    # yarn tslint --project src/js/fable-core
    yarn tsc --project src/js/fable-core
    popd

    pushd ../js/fable-core
    dotnet restore
    bash quickfsbuild.sh --no-build
    popd
    dotnet restore
fi

pushd ../dotnet/Fable.Compiler
dotnet run --no-build yarn-splitter \
    --cwd ../../tools \
    --fable-core ../../../build/fable-core \
    # --args "${ARGS/--build-core/}"
popd
node temp/QuickTest.js
