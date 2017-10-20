#!/bin/bash
if test "$OS" = "Windows_NT"; then
  MONO=""
else
  # Mono fix for https://github.com/fsharp/FAKE/issues/805
  export MONO_MANAGED_WATCHER=false
  MONO="mono"
fi

$MONO .paket/paket.exe update

exit_code=$?
if [ $exit_code -ne 0 ]; then
exit $exit_code
fi
$MONO packages/build/FAKE/tools/FAKE.exe $@ --fsiargs build.fsx
