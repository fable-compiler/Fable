#!/bin/sh -x

dotnet tool restore
dotnet run --project src/Fable.Build/Fable.Build.fsproj -- $@
# dotnet run --project src/Fable.Build/Fable.Build.fsproj -- test javascript
# dotnet run --project src/Fable.Build/Fable.Build.fsproj -- test typescript
