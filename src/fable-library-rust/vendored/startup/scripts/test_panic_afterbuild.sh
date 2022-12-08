#!/bin/bash
# don't call me directly, call from test.sh or in CI
if test "$CI" = "1"; then
    set -v
fi

# run in subshell to avoid getting killed by the signal
# note: `trap` doesn't help here.
# shellcheck disable=SC2091
$(./testcases/target/debug/dylib_runner testcases/target/debug paniclib)
status="$?"
echo "note: paniclib exited with $status (should be an error)"
if test "$status" -eq "0"; then
    echo "paniclib should not successfully exit" >&2
    exit 1
fi

if test "$status" -eq "99"; then
    echo "paniclib should abort, not catch the panic" >&2
    exit 1
fi

echo "panic test passed!"
