# Performance frontiers

Fickl owns the whole stack it runs on: the Fable compiler, FCS (fickle-fsharp `fickle_slim`), the FSharp.Core the
compiler itself runs on, and the .NET 10 host configuration. This file lists every place left to look, not only the
ones the current profile points at, so priorities can be set from measurements.

The FSharp.Core here is the compiler's own runtime. User projects type-check against the FSharp.Core they reference
from NuGet, so ours can change freely, provided its public surface stays binary-compatible: Fable plugins load into
the compiler process and bind against it.

Status: done, open (not tried), measured-no-gain, or ruled out (with the reason).

Runtime sweeps use cold `--noCache` runs, configurations interleaved, 4–5 runs each, and compare the compile phase
("Fable compilation finished in"), which excludes project cracking. Glutinum's compile times range 4.2–6.8 s within one
configuration, so it is not used to rank settings; the JS test suite stays within ±0.2 s.

## 1. Host and runtime configuration (no source changes)

| Lever | Status | Notes |
| --- | --- | --- |
| Server GC | done | Already set upstream. |
| `TieredCompilation.CallCountingDelayMs=0` | done | Fork setting. |
| ReadyToRun FCS | measured-no-gain | Same wall time; jitting runs on idle cores. |
| GC gen0 budget (`GCgen0size=256 MB`) | measured-worse | JS suite compile 3.6 → 5.2 s despite 49 → 21 gen0 GCs. |
| DATAS off (`GCDynamicAdaptationMode=0`) | measured-no-gain | 49 → 4 GCs, compile 3.60 → 3.47 s (noise). GC count is not what costs time. |
| Concurrent (background) GC off | measured-no-gain | Within noise. |
| Invariant globalization | measured-no-gain | Within noise on the JS suite. |
| TieredPGO off | measured-mixed | Compile phase, median of 5: JS suite 3.83 → 3.19 s, Python suite 3.16 → 2.62 s, Sutil 4.88 → 4.63 s, Ionide 3.02 → 3.06 s, Fantomas tools 4.47 → 4.85 s (slower in every run). Not made the default. |
| TieredPGO off + DATAS off | measured-mixed | Best on four projects, Fantomas tools 4.47 → 5.22 s. |
| Full JIT up front (`TieredCompilation=0`) | measured-worse | JS suite 3.83 → 3.39 s, Ionide 3.02 → 3.90 s. Exposed an output-dir race, fixed in `cbd6e3fc7`. |
| QuickJitForLoops off (with PGO off) | measured-no-gain | No better than PGO off alone. |
| Native AOT | open, low | Reflection-heavy (FCS, plugins); JIT is off the critical path. |

## 2. FSharp.Core (compiler runtime)

| Lever | Status | Notes |
| --- | --- | --- |
| Target net10.0 instead of netstandard2.x | open | Unlocks span/vectorised BCL paths under `String`, `Array`, hashing. |
| Generic equality / comparison / hashing fast paths | open | FCS leans on structural equality; check its share in a profile. |
| `Map` / `Set` implementation | open | Used throughout FCS name-resolution environments. |
| `List` / `Seq` / `Array` hot functions | open | Profile-driven. |

## 3. FCS

### Where type checking goes on real projects

`FICKL_FCS_ACTIVITIES=<csv>` with `--timings` (`23634f260`) records FCS's per-file check activities. Cold `--noCache`
compiles, one run each; "package" files are the sources Fable copies into `fable_modules` and checks ahead of the
project's own files.

| Project | Check wall | Package files, summed | Own files, summed | ≤1 file in flight | Largest packages |
| --- | --- | --- | --- | --- | --- |
| Fantomas tools | 5.0 s | 56 files, 6.6 s | 42 files, 1.0 s | 3.5 s | Feliz 3.9 s, Elmish 0.8 s, Fable.Promise 0.6 s |
| Sutil | 3.7 s | 60 files, 8.0 s | 60 files, 2.7 s | 1.6 s | Fable.Formatting.Markdown 1.7 s, Feliz.Engine.Bulma 1.5 s, Feliz.Engine 1.3 s |
| Ionide | 2.6 s | 14 files, 2.5 s | 32 files, 1.8 s | 1.7 s | Fable.Promise 1.1 s, VS Code bindings 0.6 s |
| Glutinum | 4.7 s | 37 files, 1.8 s | 66 files, 5.2 s | 3.5 s | Fable.Promise 1.1 s, FsToolkit 0.7 s |
| `tests/Js/Main` | 2.3 s | 10 files, 2.1 s | 66 files, 17.0 s | 0.1 s | — |

- Package sources are most of the type-checking work on three of the four external projects, and they are identical
  on every run of a given package version. In Fantomas tools the serial stretch is Feliz's own file chain
  (`Styles.fs` → `Properties.fs` → `React.fs` → `Html.fs` → `JSX.fs`).
- `fsc` checks Fantomas tools' own 48 files in 1.3 s summed with a 0.37 s critical path; the gap to Fable's 5.0 s is the
  package sources, not slower checking.
- Fable.Promise costs 0.6–1.1 s for 2–4 files wherever it is used.
- The JS test suite has almost no package sources and a wide graph, which is why it gained most from parallel checking.

| Lever | Status | Notes |
| --- | --- | --- |
| Cache type-checked package sources across runs | done | `src/Fable.Cli/PackageCache.fs`; results below. |
| Package sets that cannot be one assembly | done | Sutil's packages declare both a module and a type `Feliz.Util`; FCS type-checks that but cannot emit it. Such sets are now cached as one assembly per package (`Fable.Precompiled.<n>.dll`, each referencing the ones before it), with one merged precompiled info. Sutil compile 4.8 → 2.1 s on a hit. |
| Package-like sources outside `fable_modules` | open | Ionide's VS Code bindings come from `paket-files` as plain `Compile` items and are not cached. |
| Cache build blocks the watcher on a miss | open | ~3.5 s once per package set and options; could run in the background. |
| Files waiting on `Promise.fs` at the start of checking | explained | Fable.Promise's `PromiseImpl.fs` is a root-level `[<AutoOpen>] module` that calls `Promise.PromiseBuilder()`, so the graph correctly makes every later file depend on it and, through it, on `Promise.fs`. A package cache hit removes it: in Ionide checking starts at 0 ms instead of 1.0 s and the check wall goes 2.55 → 2.19 s; the critical path is then `Fable.Import.VSCode.fs` (1.07 s, from `paket-files`). |
| Fable.Promise check cost | open | 0.6–1.1 s per project; find what in it is slow to check. |
| IWSAM-as-type check collecting extension methods | done | fickle-fsharp `594a55f8b`. `CheckIWSAM` runs for every interface named in a signature and gathered all methods in scope, extension methods included, only to drop extension members: 44% of checking Glutinum's 18.7k-line `TypeScript.fs` (stock `fsc` spends the same 3.0 s on it). Now intrinsic methods only; same warnings. `TypeScript.fs` 3.2–3.5 → 1.9–2.0 s, Glutinum compile 4.4–5.0 → 3.4 s, Ionide 3.0–3.5 → 2.5 s, Sutil −0.2 s, JS suite unchanged; output identical. Upstreamable to dotnet/fsharp. |
| Large binding files: what remains | open | After the fix, `TypeScript.fs`'s profile is IL metadata reads for imported generic parameters (~12%), attribute checking (~12%), post-inference checks (~10%). |
| Codegen witness allocation | done | `7687bf609`. |
| Parallel cold type check over the file graph | done | `ac0a7b8c5`. Gain depends on how wide the project's graph is. |
| Symbol API caching | done | `7d83faa66`. |
| Watch mode: recheck only dependents | done | `fff58bb81`. |
| Watch mode: skip dependents when the edited file's signature is unchanged | open | Most edits change no signature; dependents need no recheck then. |
| Critical path of the type check (deep chains, largest file) | open | Measure per project. |
| Type checker hot paths (name resolution, constraint solving) | open | Needs a profile on an external project. |
| Assembly import (`TcImports`, IL and pickled signature reading) | open | Grows with package count. |
| Persisted type check results across processes | open, large | Today every new process re-checks every file even when Fable's cache is warm. |
| `arrayPathEq` / entity-ref equality | open | Span-based compare. |
| Remaining allocation (`FSharpExprConvert`, type hashing) | open | 2.9 GB in `FSharpExprConvert` before the symbol caches. |
| Parser static-constructor contention | ruled out | At most ~0.1 s. |

### Experiment: Fantomas tools against precompiled packages

A stub project with Fantomas tools' package references and one empty file, `fable precompile`d (9.3 s, once), then
Fantomas tools compiled with `--precompiledLib` pointing at it. Cold `--noCache`, 3 interleaved runs:

| | Without | With precompiled packages |
| --- | --- | --- |
| FCS parse + type check (wall) | 3.47–3.75 s | 0.90–0.99 s |
| Compilation total (wall) | 3.73–4.06 s | 1.06–1.17 s |

- The project's own 40 JS files have identical bodies; only import paths differ, because the package JS lives in the
  precompiled folder (`../../pc-fantomas/fable_modules/...`) instead of `./fable_modules/...`.
- Pointing `--precompiledLib` at the output folder itself does not work: a fresh crack wipes `outDir/fable_modules`
  before the precompiled info is read.

How upstream's precompile works (for building on it):

- `fable precompile` runs a normal compile, then writes `fable_modules/Fable.Precompiled.dll` (FCS `Compile` over every
  source), chunked inline-expression JSON, and `precompiled_info.json` (Fable version, options, source → root module and
  JS out path) (`Main.fs` 1675–1741, `Util.fs` 878–997).
- `--precompiledLib <dir>` references the DLL and drops the listed sources, matching package sources by their path from
  `fable_modules` on (`ProjectCracker.fs` 824–888); inline bodies load lazily (`State.fs` 579–590); imports of
  precompiled files rewrite to their JS out path, JavaScript only (`Pipeline.fs` 122–129).
- Limits: one precompiled lib per compilation; Fable version must match exactly, but defines, FSharp.Core and Fable.Core
  versions are not checked; source changes only warn (timestamp).

### Package cache (`PackageCache.fs`)

On by default for JavaScript, TypeScript, Python and BEAM; `--noPackageCache` turns it off, `FICKL_PACKAGE_CACHE` moves it
(default `%LOCALAPPDATA%/fickl/packages`, `~/.local/share/fickl/packages` on Linux).

- Key: Fickl and Fable versions, compiler options, fable-library path, exclude/replace, the F# options with each
  referenced assembly's size and timestamp, and every package source's path from `fable_modules` on and its content.
- Miss: after a successful compile, an FCS compile of the package sources alone (under its own project name, so its
  signature data is not named like the consumer's) plus their inline bodies and JS, written to a temporary folder and
  moved into place. Package sets FCS cannot emit as one assembly are compiled again as one assembly per package
  (listed in `<key>/groups.txt`, named apart because FCS identifies an assembly by its name); only when that fails
  too is the key marked `<key>.uncacheable`.
- Hit: reference the cached assembly, drop the package sources, copy their JS into `outDir/fable_modules`, rewrite out
  paths and the fable-library dir, and point inline bodies at this project's copies of their files, so imports resolve
  as in a source compile.
- Two upstream precompile bugs found on the way: overloads taking `Func<_, _>` and the lambda it wraps get the same
  unique name, and `fable precompile` keeps whichever inline body comes last in completion order; the cache keeps the
  last one in file order, as a source compile does.

Cold `--noCache` compile phase, one run each, output compared file by file with a source compile (identical for all
but Sutil, where one eta-expanded lambda's parameter is named after the cached member's parameter, `value_2` instead of
`children`):

| Project | Package files | Miss (compile + one-time cache build) | Hit |
| --- | --- | --- | --- |
| Fantomas tools | 52 | 3.1–3.9 s + 3.5–3.9 s | 1.01–1.25 s |
| Glutinum | 37 | 4.2 s + 1.5 s | 3.7 s |
| Ionide | 10 | 2.6 s + 0.9 s | 2.3 s |
| `tests/Js/Main` | 1 | 3.1 s + 0.4 s | 3.2 s |
| Sutil | 31 | 4.8 s + 6.6–7.4 s (11 assemblies) | 2.1 s |

Also checked: runs without `--noCache` (cached project options, a skipped up-to-date compile, an edit), a hit after
the folder that built the entry was deleted, and watch mode (miss, then hit: identical output, 240–275 ms rebuilds).

#### Python and BEAM

- Python: imports of a cached file resolve through its out path in this project (`Pipeline.fs`, `PythonFileWriter`),
  as JS does, because the entities of a cached assembly record the source path where the entry was built. A Python
  library's `__init__.py` files are written on a hit. The project's output type is in the key: Python declares a
  library's `__all__` in package sources too.
- BEAM: module names come from a source's path within its package, so imports need no rewriting. The module-name
  clash check also covers cached package sources.
- Package `.fsi` files are type-checked but compile to nothing; they no longer stop an entry from being saved (this
  affected JS too, e.g. FSharp.Data.Adaptive).
- Output checked against a `--noPackageCache` compile for a miss, a hit from another folder and a hit in place: BEAM
  byte-identical; Python identical once the `_arrowN` / `ObjectExprN` numbering is ignored, which also differs
  between two source compiles (a counter shared by files compiled in parallel). A hit from another folder can order
  Python imports differently: they are sorted by their raw path, which for cached entities is the builder's. Python
  output ran with the same results; BEAM output was not run (no Erlang on the machine).
- Python test suite: 2602 passed.

Bench projects (scratchpad): Python = FsToolkit.ErrorHandling, Hedgehog, Thoth.Json.Core, Fable.Python (104 package
files); BEAM = FsToolkit.ErrorHandling, Hedgehog (65). Cold `--noCache`, 5 alternating runs, median:

| Target | Upstream Fable 5.22: wall / compile phase | Fickl, no package cache | Fickl, cache hit |
| --- | --- | --- | --- |
| Python | 16.2 s / 13.7 s | 12.0 s / 9.3 s | 4.9 s / 2.1 s |
| BEAM | 14.3 s / 11.3 s | 12.3 s / 9.4 s | 5.1 s / 2.1 s |

A miss costs a one-time 8–9 s entry build on top of the compile.

## 4. Fable

| Lever | Status | Notes |
| --- | --- | --- |
| Fable transforms and printing | open, low | Runs beside FCS on spare cores; not on the critical path with many cores. |
| Project cracking on a cold cache | open | 2–3 s per first run (MSBuild design-time build); cached afterwards. |
| Watch mode fixed overhead | open | ~230–300 ms rebuilds on small edits; find what the floor is made of. |
| Process startup and assembly loading | open | Measure. |
