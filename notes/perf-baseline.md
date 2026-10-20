# Performance baseline

Measured with `dotnet run --project src/Fickl.Build -- timings` (the `--timings` report), Release compiler, cold process, `--noCache`.

## 2026-10-20: tests/Js/Main → JavaScript (76 source files), Windows 11, .NET 10

| Phase | Default (parallel type check) | `--no-parallel-type-check` |
| --- | --- | --- |
| Project cracking (wall) | 2.9–5.5 s | 3.0 s |
| FCS parse + type check (wall) | 8.1–9.6 s | 7.4 s |
| Fable per-file work (summed across threads) | 8.0–9.6 s over 75 files | 30.0 s over 74 files |
| Compilation total (wall, excludes cracking) | 8.2–9.8 s | 9.1 s |
| Allocated | ~16 GB | ~16 GB |
| GC gen0/gen1/gen2 | ~105/50/9 | 138/37/10 |

Ranges span the runs taken while building the tool; cracking varies most (MSBuild evaluation).

### Reading it

- **FCS is the critical path.** With the default parallel type check, Fable compiles each file as soon as FCS
  hands it over, and the compilation finishes ~0.2 s after the type check does. Fable's own work hides
  almost entirely behind FCS.
- **Fable work parallelises well.** With the type check run first, Fable's 30 s of summed per-file work completes
  in ~1.8 s of wall time. (Per-file times inflate under that contention, which is why the sum is higher than
  in the default mode.)
- **Allocation is very high**: ~16 GB for 76 files, with 8–10 gen2 collections. Most of it is expected to be
  FCS; a dotTrace allocation snapshot should confirm before acting on it.
- **Project cracking** (MSBuild design-time evaluation) is the second-largest cost and is outside FCS.

## 2026-10-20: same benchmark, net10-only FCS (fickle-fsharp `da62eba`)

FCS rebuilt for net10.0 only (`dotnet run --project src/Fickl.Build -- fcs-fork`), with no source changes.
Default mode, 3 runs:

| Phase | netstandard2.0 FCS (above) | net10 FCS |
| --- | --- | --- |
| Project cracking (wall) | 2.9–5.5 s | 3.6–4.0 s |
| FCS parse + type check (wall) | 8.1–9.6 s | 8.4–10.0 s |
| Fable per-file work (summed) | 8.0–9.6 s | 7.8–8.7 s |
| Compilation total (wall) | 8.2–9.8 s | 8.6–10.1 s |
| Allocated | ~16 GB | ~11 GB |
| GC gen0/gen1/gen2 | ~105/50/9 | ~70/44/8 |

- Wall time is unchanged within noise: the retarget alone buys no speed.
- Allocation drops by about a third. The profile below shows why; it is not the BCL.

## 2026-10-21: profile of the same benchmark (net10 FCS)

One dotTrace timeline snapshot for the overview, and dotnet-trace for the split by phase: CPU samples
(`dotnet-sampled-thread-time`) plus GC allocation ticks (`Microsoft-Windows-DotNETRuntime:0x1:5`).
The allocation ticks sum to 11.1 GB, which matches the `--timings` figure.

### CPU by phase

Running samples only, attributed by the innermost phase on the stack. "Fable → FCS" is FCS code
that Fable calls through the symbol API, which FCS runs lazily during Fable's transform.

| Phase | Share of running CPU |
| --- | --- |
| FCS parse | 22% |
| FCS type check | 21% |
| Fable's own code | 13% |
| Fable → FCS symbol API | 10% |
| Fable → FCS `FSharpExprConvert` | 5% |
| FCS import (TcImports, IL reading) | 4% |
| Other (mostly a blocked read of the MSBuild cracker's stdout) | 21% |

- **About 40% of the parse share is waiting.** Parallel parse threads hit `FSharp.Compiler.Parser`'s static
  constructor (the fsyacc tables) together and spin in `InitClassSlow` while one thread runs it: 8.5% of
  running samples under parsing, 10.5% across all phases.
- **GC suspension shows up as running time**: `Thread.PollGC` is about 13% of running samples across phases.
- **JIT is not on the critical path.** The timeline shows 19.8 s of jitting over 65,725 methods, but a
  ReadyToRun publish (FCS 19 → 38 MB) compiles in the same wall time, 3 runs each (8.0–8.6 s JIT,
  8.1–8.4 s R2R). The jitting is tier-up on idle cores. This rules out R2R as a speedup here.

### Allocation by phase

| Phase | Allocated |
| --- | --- |
| FCS type check | 3.2 GB (29%) |
| Fable → FCS `FSharpExprConvert` | 2.9 GB (26%) |
| Fable → FCS symbol API | 2.3 GB (21%) |
| Fable's own code | 1.5 GB (14%) |
| FCS import | 0.8 GB (8%) |
| FCS parse | 0.1 GB (1%) |

- **1.95 GB (18% of all allocation) is one bug.** `FSharpExprConvert.GetWitnessArgs` runs on every value
  or member application. It calls `ConstraintSolver.CodegenWitnessesForTyparInst`, which builds a fresh
  `InfoReader` through `CreateCodegenState`, and that preallocates a dozen `Cache`s, each backed by a
  `ConcurrentDictionary` of about 1,000 buckets. The `InfoReader` is built before checking whether any
  type parameter has a trait constraint, and almost none do. Fix candidate (fickle-fsharp,
  `src/Compiler/Checking/ConstraintSolver.fs`): return `[]` early when
  `GetTraitConstraintInfosOfTypars g typars` is empty. The same `CreateCodegenState` sits under
  IlxGen's and the Optimizer's witness paths.
- The rest of the FCS-side allocation is spread thinly: type-structure hashing (`TypeHashing.emitTType`),
  `FSharpType.isUnresolved`, `CurriedParameterGroups`, and IL metadata rows read via tuples.

### Why the net10 build allocates 5 GB less

The current Fable build, run against the old netstandard2.0 `lib/fcs` binaries (only the FCS assemblies
swapped), reproduces 16.0 GB. Traced, the difference is two closures that the old binary allocates and the new one does not:

- `primEntityRefEq@471`: 3.4 GB (`TypedTreeBasics.fs`, called by every entity-ref equality test)
- `arrayPathEq` `loop@397`: 1.07 GB (the inner `let rec loop` over the path arrays)

Neither function has platform-specific code. Both binaries are optimised builds, but closure names differ
(`loop@235-42` vs `loop@233-47`), so the old binary is a different source revision and possibly a
different compiler. Which of the two removed the closures has not been isolated.
`arrayPathEq` is still a candidate for a span-based `SequenceEqual` rewrite.

### Witness fix applied (fickle-fsharp `7687bf609`)

`CodegenWitnessesForTyparInst` now returns no witnesses before building the codegen state when no
type parameter has a trait constraint. 3 runs:

| | Before | After |
| --- | --- | --- |
| Compilation total (wall) | 8.6–10.1 s | 8.0–8.6 s |
| Allocated | ~11.0 GB | ~8.7 GB |
| GC gen0/gen1/gen2 | ~70/44/8 | ~59/17/6 |

Generated output is unchanged: `tests/Js/Main` JavaScript is byte-identical (141 files), and
`tests/Python` is identical once the nondeterministic `_exprN`/`_arrowN`/`ObjectExprN` counters are normalised (110 files).
Warnings are identical. Compiler tests pass 133/133.

### Wall-clock view: type checking is serial

Sample timestamps per phase, from the same trace (ms from trace start):

| Phase | Wall span | Threads |
| --- | --- | --- |
| Project cracking (`run_dotnet_msbuild`) | 0–3,543 | 5 |
| FCS parse, all 76 files (`ParseInput`) | 4,755–5,135 (0.4 s) | 24 |
| FCS type check (`TypeCheckOneInputEntry`) | 5,138–13,976 (8.8 s) | **1** |
| Fable transform (`transformFile`) | 6,147–14,031 | 8 |

- Type checking is the critical path, and it runs on one thread, file after file. Fable's transforms
  finish 55 ms after the last file is checked.
- Parsing is a 0.4 s burst. Its large CPU share is 24 threads at once, and the parser static constructor
  costs only 60–75 ms cold (measured in isolation). Removing that contention would save at most ~0.1 s
  of wall time, so it is not worth a fork change.

### How much graph-based checking could save

`fsc` on the same project (`dotnet build tests/Js/Main --no-dependencies --no-incremental`), with
`--times:<csv>`, with and without `--test:GraphBasedChecking`. Spans of `CheckDeclarations.CheckOneImplFile`, 69 files:

| Mode | Type check wall | Summed per-file time | Effective parallelism |
| --- | --- | --- | --- |
| Sequential | 8.3 s | 8.3 s | 1.0 |
| Graph | 2.0 s | 13.9 s | 6.9 |

- The dependency graph of the test project is wide: type checking wall time drops by 4x.
- Per-file times rise by ~70% under contention, and the floor is `ConvertTests.fs` (1.3–1.6 s on its own).
- `fsc` checks with the same type checker as Fable's `service_slim`, so this bounds what parallel checking
  there could save on a cold compile: about 6 s of the ~8.5 s compilation.

### Parallel type checking applied (fickle-fsharp `ac0a7b8c5`)

A cold `ParseAndCheckProject` now checks files over the dependency graph
(`CheckMultipleInputsUsingGraphModeWithStates`), when the check cache is empty and the project has not
opted out of graph checking. Files reach Fable in project order, each as soon as it and every file before it are
checked: releasing them in dependency order instead made Fable's output non-deterministic (external
`DllRef/Lib.js` vs `DllRef_1/Lib.js` naming) and broke a root-module lookup that expects earlier files to be known.
The check cache gets the same per-file states a sequential check leaves, so watch-mode rechecks stay sequential
from the last unchanged file.

| Benchmark | Before | After |
| --- | --- | --- |
| `tests/Js/Main` → JS, compilation total (wall) | 7.4 s | 3.0–4.1 s (3 runs) |
| `tests/Python` → Python, compilation total (wall) | 5.6 s | 2.9 s |
| Allocated (JS) | ~8.8 GB | ~9.2 GB |

- JS output is byte-identical (141 files) and Python is identical after counter normalisation (110 files);
  warnings are identical. Compiler tests pass 133/133.
- Watch mode: after editing `StringTests.fs`, the recheck (0.9 s) matches a cold watch-mode compile of the same
  source, file for file.

### Project cracking is cached

The 2.6–4.3 s of project cracking in the tables above is with `--noCache`. Without it, Fable reads the cracked
project from its cache: 119 ms when nothing changed, 113 ms after editing a source file. A full crack only happens
on the first run or after a project file changes, so it needs no fork work.

### Symbol API caching (fickle-fsharp `7d83faa66`)

A fresh trace of the parallel-checking build ranked the FCS symbol API calls Fable makes by allocation. The top ones
recomputed the same answer on every call:

- Every `FSharpType` predicate (`IsTupleType`, `HasTypeDefinition`, `IsAbbreviation`, ...) first checked resolution
  by building an `FSharpEntity`; `IsAbbreviation` did so two or three times per call (408 MB).
- `FSharpType.GenericArguments` built a list, an array and a collection per call (192 MB, 5.8% of CPU), and Fable
  reads it several times per type.
- `CurriedParameterGroups` (586 MB) is read several times per member by Fable.

`FSharpType` now caches its resolution and generic arguments and checks the definition reference directly;
`FSharpMemberOrFunctionOrValue` caches its parameter groups.

| | Before | After |
| --- | --- | --- |
| Allocated, `tests/Js/Main` → JS | 9.26 GB | 7.94 GB |
| Allocated, `tests/Python` → Python | 8.58 GB | 7.52 GB |
| Fable per-file work (summed, JS) | 13.9 s | 12.2 s |
| Compilation total (wall, JS, median of 6) | 3.42 s | 3.34 s |

- Output is identical (JS byte-for-byte, 141 files; Python after counter normalisation, 110 files), as are warnings.
  Compiler tests pass 133/133.
- Wall time does not move on this machine: FCS type checking (2.7–3.0 s) is the critical path, and Fable's work
  runs beside it on spare cores. The CPU and allocation savings should show as wall time on machines with fewer cores.

### Dependents-only recheck in watch mode (fickle-fsharp `fff58bb81`)

Every check of several files now goes through the graph checker. Each file's result is kept and reused while the
file and everything it depends on (in the current or the previous dependency graph) are unchanged, so an edit
rechecks only the edited file and its dependents, in parallel.

One watch session of `tests/Js/Main`, four edits in a row; "Fable compilation finished" time per edit:

| Edit | Sequential recheck (before) | Dependents-only recheck |
| --- | --- | --- |
| Add a value to `StringTests.fs` (few dependents) | 0.94 s | 0.61 s |
| Add a value to `Util/Util.fs` (most files depend on it) | 3.6 s | 1.3 s |
| Type error in `StringTests.fs` | 0.39 s | 0.46 s |
| Revert both files | 3.7 s | 1.1 s |

- Each recheck's output is identical to the sequential recheck's, file for file, and the type error is reported the
  same way.
- Compared with a cold compile of the same sources, both rechecks differ only in `MiscTests.js` whenever it is
  rechecked: FCS gives closures of a `let rec` a new numbered name (`recursive1@385-2`) each time a file is checked
  again in the same process. This is existing behaviour, not new.
- Cold compiles are unchanged (byte-identical JS, identical warnings). Compiler tests pass 133/133.

### Next measurements

- The same tables for Python and for a large external project.
- (Dropped) Parser static-constructor contention: initialise the parser tables once, before the parallel parse starts.
- The same table for Python and for a large external project.

### Profiling notes

- `dottrace start` and `dotnet-trace collect -- <cmd>` both hang this benchmark. Project cracking
  starts `dotnet msbuild`, which apparently inherits the profiler's suspend-on-start diagnostic port and waits forever.
  Start Fable normally and attach by PID instead (`dotnet-trace collect -p <pid>`).
- Rider's dotTrace MCP tools time out on a 530 MB timeline snapshot of this run.
