# Performance baseline

Measured with `dotnet run --project src/Fickle.Build -- timings` (the `--timings` report), Release compiler, cold process, `--noCache`.

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

FCS rebuilt for net10.0 only (`dotnet run --project src/Fickle.Build -- fcs-fork`), with no source changes.
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

### Next measurements

- Parser static-constructor contention: initialise the parser tables once, before the parallel parse starts.
- The same table for Python and for a large external project.

### Profiling notes

- `dottrace start` and `dotnet-trace collect -- <cmd>` both hang this benchmark. Project cracking
  starts `dotnet msbuild`, which apparently inherits the profiler's suspend-on-start diagnostic port and waits forever.
  Start Fable normally and attach by PID instead (`dotnet-trace collect -p <pid>`).
- Rider's dotTrace MCP tools time out on a 530 MB timeline snapshot of this run.
