@echo off

rem Fickle's build CLI (Partas.Build). Upstream's build.bat still works unchanged.
dotnet tool restore
dotnet run --project src/Fickle.Build/Fickle.Build.fsproj -- %*
