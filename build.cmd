@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

utils\fnr\fnr.exe --cl --find "FSharp.Compiler.Service.ProjectCrackerToolCopy" --replace "FSharpCompilerServiceProjectCrackerToolCopy" --dir "%cd%" --fileMask "*.targets" --includeSubDirectories

packages\FAKE\tools\FAKE.exe build.fsx %*
