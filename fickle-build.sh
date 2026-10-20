#!/bin/sh

# Fickle's build CLI (Partas.Build). Upstream's ./build.sh still works unchanged.
dotnet tool restore
dotnet run --project src/Fickle.Build/Fickle.Build.fsproj -- "$@"
