@echo off
cls

.paket\paket.exe update
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*
