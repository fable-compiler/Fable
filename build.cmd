@echo off
cls

.paket\paket.exe restore --silent
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*
