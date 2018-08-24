#!/bin/bash
# First time after cloning the repository, run:
#   bash quicktest.sh --build-core

# Next time, if src/js/fable-core files haven't changed, just:
#   bash quicktest.sh

# The dummy arg is necessary to prevent failure of dotnet SDK 2.1.200 CLI arg parsing
ARGS="$@ --dummy"

echo "dotnet SDK version"
dotnet --version

if [ "${ARGS/--build-core/}" != "$ARGS" ]; then
    echo "Building fable-core..."

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
    sh quickfsbuild.sh
    popd
    dotnet restore
fi


if [ "${ARGS/--build-js-core/}" != "$ARGS" ]; then
    echo "Building JS fable-core..."

    pushd ../..
    # yarn tslint --project src/js/fable-core
    yarn tsc --project src/js/fable-core
    popd
fi

dotnet run -p ../dotnet/Fable.Compiler \
    yarn-build --force-pkgs --args "$ARGS"
