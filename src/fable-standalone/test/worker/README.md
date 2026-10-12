# Worker harness

Drives `dist/worker.min.js` from Node, over the same Thoth-encoded protocol it speaks in a browser
(`src/Worker/Shared.fs`). The worker only needs four browser globals - `self`, `importScripts`,
`postMessage` and `fetch` - so a `vm` context is enough to run the real published artifact outside a
browser, which is otherwise the only place it can be exercised.

It runs as part of `./build.sh test standalone`, which is what CI calls. To iterate on it directly:

```bash
./build.sh standalone                 # or worker-js, to rebuild just the worker
node src/fable-standalone/test/worker/run.mjs
```

Options:

| Option | Meaning |
| --- | --- |
| `--files 12` | how many files to compile (default 6) |
| `--dist <path>` | a different build, e.g. an unpacked npm tarball |
| `--metadata <path>` | assemblies directory (defaults to `src/fable-metadata/lib`) |

The fixture is generated, so nothing is downloaded.

## What it checks

Beyond "the compile finished and returned a module per file", it measures the **stack headroom the
compiler is handed for each file** and fails if it drifts between files.

That is not an abstract worry. A browser worker gets a smaller stack than Node does - roughly what
`node --stack-size=700` gives - and Fable's JS `Async` runs each bind's continuation inside the
caller's frame, unwinding only every 2000 of them (`Trampoline.maxTrampolineCallCount`). Printing a
file binds once per top-level declaration, so before this was fixed each file was handed whatever
the previous file's printing had left:

```text
headroom fell from 12501 to 6165 frames across 6 files (12501, 10389, 8277, 6165, 11151, 9039)
```

Half the stack gone, and then the deeply recursive `CompileToTargetAst` runs on what remains. In
Node that only showed up under `--stack-size=400`; in a browser worker it was a
`CompilerCrashed: Maximum call stack size exceeded` that no one could reproduce off-browser.

To see the margin directly, lower Node's stack to something browser-shaped:

```bash
node --stack-size=300 src/fable-standalone/test/worker/run.mjs
```
