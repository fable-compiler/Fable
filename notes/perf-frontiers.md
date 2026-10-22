# Performance frontiers

Fickl controls the Fable compiler, FCS (fickle-fsharp `fickle_slim`), and the .NET 10 host configuration.
FSharp.Core is also an optimization frontier, but the 0.5.3 tool currently resolves its runtime from
NuGet (`FSharp.Core/10.1.203`), not the older unsigned DLL in `lib/fcs`. This file lists every place
left to look, not only the ones the current profile points at, so priorities can be set from measurements.

The FSharp.Core discussed here is the compiler's runtime. User projects type-check against the
FSharp.Core they reference from NuGet. A runtime fork must preserve the actual shipped assembly
identity, API and behavior: Fable plugins also load into the compiler process and bind against it.

Status: done, open (not tried), measured-no-gain, or ruled out (with the reason).

Fickl's NuGet release uses `dotnet run --project src/Fickl.Build -- publish` with `FICKL_NUGET_KEY`
provided in the environment (`--pack-only` builds and validates without uploading). The single
`FicklVersion` property in `src/Fickl.Branding.targets` versions Fickl.Core, Fickl.AST, Fickl.Compiler,
Fickl.PublishUtils and the CLI packages together, while retaining upstream assembly versions for binary
compatibility with existing plugins. EasyBuild's changelog-derived package version is overridden after its
version target runs. Generated runtime libraries are rebuilt before packing, and
every tool runtime package is checked for all bundled language libraries. Companion packages and all runtime packages are pushed before
the tool pointer package. Compiler symbols are embedded; its empty separate symbol archive is not pushed.

The initial 0.5.2 release incorrectly applied the package version to companion assembly versions, which
broke existing Feliz plugins. The published 0.5.3 correction preserves those assembly identities and restores
the missing Rust and Dart runtime payloads. Validation includes 141 compiler tests, the assembly-qualified
AST reference used by older plugins, a consumer of all four companion packages, and a Fantomas tools
compile using its existing Feliz plugins. Fickl.Core is the separate `src/Fickl.Core` project containing
Fickl-specific attributes, not a renamed Fable.Core package; its consumer check resolves
`Fickl.Core.CaseTestAttribute`. The mistaken rename in 0.5.2 also caused duplicate project identities when
restoring the JS suite. All 13 corrected packages were published on 2026-10-22. NuGet rejected the
publishing key's unlist request with HTTP 403; 0.5.2 remains listed by the maintainer's decision.

Runtime sweeps use cold `--noCache` runs, configurations interleaved, 4–5 runs each, and compare the compile phase
("Fable compilation finished in"), which excludes project cracking. Glutinum's compile times range 4.2–6.8 s within one
configuration, so it is not used to rank settings; the JS test suite stays within ±0.2 s.

## End to end against upstream (Fickl 0.5.3, 2026-10-22)

[Comparison image](benchmarks/fickl-0.5.3.png), [raw timings](benchmarks/fickl-0.5.3.csv),
[package hashes and environment](benchmarks/fickl-0.5.3-manifest.json). Wall clock from process start to exit,
five interleaved runs per configuration, fresh output directories and `--noCache`, including restores.
Cache hits are warmed; the CSV separately records warmups and one first-use cache build per workload.
All 120 measured runs succeeded. These measurements compare the full fork with upstream, not just the
incremental effect of the IWSAM optimization.

| Project | Fable 5.22 | Fable, telemetry opted out | Fickl, cache off | Fickl, cache hit | Hit vs opted out |
| --- | --- | --- | --- | --- | --- |
| Sutil | 9.20 s | 7.28 s | 3.99 s | 2.39 s | 3.0× |
| Fantomas tools | 7.62 s | 6.29 s | 2.68 s | 1.92 s | 3.3× |
| Ionide | 12.22 s | 5.60 s | 2.91 s | 2.84 s | 2.0× |
| JS suite (`--exclude Fable.Core`) | 28.28 s | 8.87 s | 4.07 s | 3.94 s | 2.3× |
| Python bench | 11.24 s | 9.89 s | 7.43 s | 1.80 s | 5.5× |
| BEAM bench | 10.58 s | 9.17 s | 7.09 s | 1.52 s | 6.0× |

The previous harness was recovered from the local Claude scratchpad. Its Windows replacement is
`notes/benchmarks/benchmark-0.5.3.py --scratch <scratchpad> --tool <packaged-fable.exe>`;
`plot-0.5.3.py` renders the CSV with matplotlib. The JS workload was resumed after correcting the
Fickl.Core project identity; packaged tool files were verified byte-identical after repacking.
Cleanup stopped leftover MSBuild workers and verified none remained; active Rider project evaluations
are excluded because Rider immediately launches replacements. Telemetry/network overhead varies by
machine and session, so the previous day's default-upstream timings are not reused.

## End to end against upstream (Fickl 0.5.1, 2026-10-21)

Same harness, adding upstream with `DOTNET_CLI_TELEMETRY_OPTOUT=1` set by the user (Fable as installed waits about 9 s
on telemetry per `dotnet` process here; Fickl 0.5.1 opts its own processes out). Wall clock, median of 5
(`harness/fair.tsv`, image `harness/fickl-0.5.1.png`):

| Project | Fable 5.22 | Fable 5.22, opted out | Fickl, package cache off | Fickl, cache hit | Hit vs opted out |
| --- | --- | --- | --- | --- | --- |
| Python bench | 17.5 s | 12.4 s | 9.0 s | 3.1 s | 4.0× |
| BEAM bench | 15.5 s | 11.6 s | 8.9 s | 3.0 s | 3.9× |
| Sutil | 18.2 s | 9.8 s | 5.7 s | 3.8 s | 2.6× |
| Fantomas tools | 14.5 s | 8.4 s | 4.2 s | 2.9 s | 2.9× |
| Ionide | 18.6 s | 7.5 s | 4.8 s | 4.0 s | 1.9× |
| JS suite (`--exclude Fable.Core`) | 50.8 s | 13.4 s | 7.0 s | 5.3 s | 2.5× |

Project loading on a hit is now 0.6–1.9 s (was 2.2–6.8 s; JS suite 28 s).

## End to end against upstream (Fickl 0.4.0, 2026-10-21)

The compile phase overstates what a user sees: project cracking and process startup are outside it. Wall clock from
process start to exit, `--noCache`, the installed 0.4.0 win-x64 tool package against upstream Fable 5.22, interleaved
in one session, median of 5 (`harness/fairbench.sh`, results in `harness/fair.tsv`):

| Project | Fable 5.22 | Fickl, package cache off | Fickl, cache hit | Cracking (both) |
| --- | --- | --- | --- | --- |
| Python bench | 12.9 s | 9.7 s | 3.7 s (3.5×) | 2.0 s |
| BEAM bench | 11.7 s | 9.3 s | 3.6 s (3.2×) | 2.1 s |
| Sutil | 11.7 s | 7.3 s | 5.1 s (2.3×) | 2.8 s |
| Fantomas tools | 8.8 s | 4.5 s | 4.0 s (2.2×) | 2.0 s |
| Ionide | 10.4 s | 7.7 s | 7.9 s (1.3×) | 5.4 s |

A cache miss costs 9.7–19.8 s once (compile plus building the entry). Glutinum and `tests/Js/Main` failed to compile
with both compilers in this setup (Glutinum's own `CssModules.fs`; `Fable.Core.Quotations` for the test suite outside
the build script), so they are left out. On a hit, cracking is now the largest part of every project's time.

## 1. Host and runtime configuration (no source changes)

| Lever | Status | Notes |
| --- | --- | --- |
| Server GC | done | Already set upstream. |
| `TieredCompilation.CallCountingDelayMs=0` | done | Fork setting. |
| ReadyToRun FCS | superseded | Same wall time on its own; jitting runs on idle cores. See the composite row. |
| GC gen0 budget (`GCgen0size=256 MB`) | measured-worse | JS suite compile 3.6 → 5.2 s despite 49 → 21 gen0 GCs. |
| DATAS off (`GCDynamicAdaptationMode=0`) | measured-no-gain | 49 → 4 GCs, compile 3.60 → 3.47 s (noise). GC count is not what costs time. |
| Concurrent (background) GC off | measured-no-gain | Within noise. |
| Invariant globalization | measured-no-gain | Within noise on the JS suite. |
| TieredPGO off (alone) | measured-mixed | Compile phase, median of 5: JS suite 3.83 → 3.19 s, Python suite 3.16 → 2.62 s, Sutil 4.88 → 4.63 s, Ionide 3.02 → 3.06 s, Fantomas tools 4.47 → 4.85 s (slower in every run). Not made the default. |
| TieredPGO off + DATAS off | measured-mixed | Best on four projects, Fantomas tools 4.47 → 5.22 s. |
| Full JIT up front (`TieredCompilation=0`) | measured-worse | JS suite 3.83 → 3.39 s, Ionide 3.02 → 3.90 s. Exposed an output-dir race, fixed in `cbd6e3fc7`. |
| QuickJitForLoops off (with PGO off) | measured-no-gain | No better than PGO off alone. |
| Native AOT | open, low | Reflection-heavy (FCS, plugins); JIT is off the critical path. |
| Composite ReadyToRun + TieredPGO off | done | Per-runtime tool packages (see below). |

### Composite ReadyToRun with TieredPGO off

With the package cache most compilations are short, and JIT becomes a large part of them: a Python cache hit spent
5.4 s of JIT (summed over threads) on a 2.2 s compile phase. `--timings` now prints process JIT time and CPU time,
which stay comparable while other work loads the machine (it did during these runs: 50–65% CPU from other processes).

Variants, interleaved, median of 4–6 runs, compile phase (process CPU in brackets):

| Workload | IL (shipped 0.3.0) | IL, PGO off | Composite R2R | Composite R2R, PGO off |
| --- | --- | --- | --- | --- |
| JS suite, `--noPackageCache` | 4.19 s (39.6) | 3.34 s (32.0) | 4.91 s (45.8) | 3.18 s (29.4) |
| Fantomas tools, `--noPackageCache` | 3.73 s (18.2) | 3.53 s (16.0) | 4.11 s (17.2) | 2.78 s (13.3) |
| Fantomas tools, cache hit | 1.61 s (12.3) | 1.50 s (10.9) | 1.09 s (9.1) | 0.90 s (7.7) |
| Python bench, cache hit | 2.60 s (7.1) | 2.27 s (5.6) | 1.19 s (4.4) | 1.06 s (3.7) |
| Sutil, cache hit | 2.76 s (18.2) | 2.62 s (15.8) | 3.27 s (18.6) | 2.33 s (13.0) |

Second set, IL against composite R2R with PGO off: Ionide hit 3.42 → 2.38 s, BEAM bench hit 1.95 → 0.90 s, Python
bench `--noPackageCache` 9.0 → 8.5 s; Glutinum hit 6.9 → 5.7 s and `--noPackageCache` 4.7 → 5.9 s, with CPU lower in
both (Glutinum's spread within one configuration is wider than these differences).

- R2R alone loses on long compilations: hot R2R methods are rejitted through an instrumented tier before the optimised
  one, so they run slow code for longer. With PGO off they go straight to the optimised tier.
- Per-assembly R2R without FCS (`PublishReadyToRunExclude`) kept most of the JIT: FCS is where the startup JIT is.
- Composite (one image of the app's assemblies, framework outside it) beat per-assembly R2R: crossgen can inline
  FSharp.Core into FCS and Fable within the image.
- Shipping: `dotnet pack` builds one package per runtime (`win-x64`, `win-arm64`, `linux-x64`, `linux-arm64`,
  `linux-musl-x64`, `osx-x64`, `osx-arm64`), each one composite R2R, a portable IL `any` package, and the `Fickl`
  pointer package; the .NET 10 SDK installs the one for the machine (`src/Fickl.Branding.targets`). The SDK turns
  composite off for framework-dependent apps, so a target sets it again before crossgen runs. Each runtime package is
  ~28 MB (IL package 13.5 MB). Publish the runtime packages before the pointer package.
- Output of the installed `win-x64` package is identical to an IL source compile (Fantomas tools). The other runtime
  packages were built (crossgen cross-compiles) but not run here.

## 2. FSharp.Core (compiler runtime)

**Status: partially explored.** Paused at the user's request after the TypeToken-array
experiment below. No experimental Core or FCS replacement has been adopted.

### Cold loading and startup checkpoint (2026-10-22)

Before moving to FSharp.Core, the packaged 0.5.3 win-x64 executable was measured in fresh
processes, with five measured runs after warmup. This is cold Fickl project-option loading
(`--noCache`), not cold OS pages or an empty NuGet cache. Package caching was disabled.
All comparison runs produced identical JS within each workload.

| Workload / mode | Process wall median ms | Project loading median ms |
| --- | --- | --- |
| `--version` | 130 | — |
| Tiny project, restore | 1,455 | 799 |
| Tiny project, `--noRestore` | 1,195 | 562 |
| JS suite, restore | 4,383 | 1,559 |
| JS suite, `--noRestore` | 4,149 | 1,252 |

The version path is only a lower-bound startup probe: it does not initialize everything needed
to compile. Process wall includes the RTK launcher. Restore and MSBuild evaluation are material
costs, but removing them would change behavior for edited imports, framework overrides and
package state. `--noRestore` remains an explicit caller choice, not a proposed default.

A separate interleaved runtime experiment set `DOTNET_TieredPGO=0` for the process tree.
The packaged compiler already sets TieredPGO false in its runtime configuration; this explored
the runtime work of its MSBuild/build subprocesses without omitting any targets or restore.
Tiny loading was 799 → 807 ms and JS loading 1,527 → 1,579 ms. Process wall medians were
1,428 → 1,440 ms and 4,295 → 4,467 ms respectively. There is no measured improvement, so
no environment default or loading behavior was changed. A larger loader redesign is not
justified by this limited experiment; these results do not close all cold-loading frontiers.

Raw timings, tiny fixture and drivers are in `notes/benchmarks/cold-start/`. The drivers use
the existing validated 0.5.3 package under `temp/release-0.5.3/verified-package/`; run them from
the worktree root. Watch latency remains deferred. The next priority is FSharp.Core changes
that reduce measured FCS work while preserving assembly identity and semantics.

| Lever | Status | Notes |
| --- | --- | --- |
| Target net10.0 instead of netstandard2.x | open | Unlocks span/vectorised BCL paths under `String`, `Array`, hashing. |
| Generic equality / comparison / hashing fast paths | open | FCS leans on structural equality; check its share in a profile. |
| `Map` / `Set` implementation | open | Used throughout FCS name-resolution environments. |
| `List` / `Seq` / `Array` hot functions | open | Profile-driven. |

### Runtime source alignment prerequisite (2026-10-22)

The first Core experiment added only `AggressiveInlining` hints to `MapTree.mk` and
`MapTree.rebalance`. The algorithm, comparator calls, insertion order and balancing rules were
unchanged. The local Core source built successfully for netstandard2.1; the unchanged upstream
MapModule/MapType tests passed all 49 cases for both baseline and candidate in an isolated
test project. The full upstream test project could not reach tests because its dependency-manager
project failed with the existing FS0074 missing `System.Array` reference error.

Before a timing comparison, the FCS process rejected the replacement Core DLL. Inspection found:

- The tool's `fable.deps.json` resolves `FSharp.Core/10.1.203`, with signed assembly identity
  `10.1.0.0`, public-key token `b03f5f7f11d50a3a`.
- `lib/fcs/FSharp.Core.dll` and the sibling checkout's default build are unsigned `10.0.0.0`.
- The CLI disables its implicit Core reference, but Thoth.Json.Core 0.10.0,
  Thoth.Json.Core.Auto 0.2.0 and Thoth.Json.System.Text.Json 0.5.0 require Core 10.1.203
  transitively. That package wins dependency resolution over the vendored reference.

An isolated public-signed build with version 10.1.0.0 allowed an identity-matched API inspection;
this was not adopted as a release configuration. The shipped Core exposed 3,156 reflected API
entries versus 3,140 in the local source build. The differences are in the LINQ `AnonymousObject`
types: the shipped types are sealed and override equality/hashing; the older source types do not.
A concrete probe confirmed that two `AnonymousObject<int>(42)` instances compare equal with the
shipped runtime and unequal with the local build. The map inlining hints are not responsible
for this difference; the baseline source is older. Merely changing version/signing would therefore
introduce an unrelated behavioral regression.

The package nuspec identifies its source as dotnet/dotnet commit
`c23858a6d860abe3ca94f9ac630c3e61b2625664`; its
[MutableTuple.fs](https://raw.githubusercontent.com/dotnet/dotnet/c23858a6d860abe3ca94f9ac630c3e61b2625664/src/fsharp/src/FSharp.Core/MutableTuple.fs)
contains the newer equality/hashing implementation. **Align Core source to the runtime package
before pursuing or shipping a Core optimization.** Preserve all runtime behavior, not just reflected
API entries; this inspection is a useful gate, not an exhaustive compatibility proof.

No Core performance result is claimed. The inlining changes were removed from source and no
vendored or packaged runtime DLL was replaced. The existing unsigned local build output was
restored. `notes/benchmarks/core-runtime/` saves the API delta, rejected experiment patch and
an executable equality probe. From the worktree root:

```powershell
rtk dotnet run --project notes/benchmarks/core-runtime -c Release -- temp/release-0.5.3/verified-package/tools/net10.0/win-x64/FSharp.Core.dll temp/core-runtime-api.txt true
```

Passing the older local build as the first argument with expected result `true` must fail.
This prerequisite was subsequently resolved by the source-aligned experiment below.

### Source-aligned Core baseline and map experiment (2026-10-22)

The VMR source manifest maps the runtime package to dotnet/fsharp commit
`7d0437e8bcf9ead9483483eaa556f1998af8806b`. The Core source directory's Git object IDs
were compared with the VMR directory at the package commit: every entry matched.
An isolated checkout of that commit builds netstandard2.1 Core with assembly version
10.1.0.0 and the runtime public key. Public signing is only for local experiments;
this is not a release configuration or a Microsoft-signed replacement.

The saved preparation script was run successfully from the worktree root:

```powershell
rtk proxy python notes/benchmarks/core-runtime/prepare-aligned-core.py
```

It requires an unchanged pinned checkout, builds the baseline, checks its assembly identity,
compares all 3,156 reflected API entries with the packaged runtime, checks anonymous-object
equality, and records hashes and source commits in `temp/core-runtime-baseline/manifest.json`.
These are targeted compatibility checks, not an exhaustive equivalence proof.

The two `AggressiveInlining` hints on `MapTree.mk` and `MapTree.rebalance` were then rebuilt
on this aligned source. Both baseline and candidate passed all 49 unchanged upstream
MapModule/MapType tests and all ten saved full compiler diagnostic/attribute snapshots.
The broader upstream Core test project was blocked by an FS0074 dependency build error
in FSharp.DependencyManager.Nuget; no full-suite pass is claimed.

Five interleaved measured fresh processes after warmups, identical CLI/FCS IL with only Core
changed, `--noCache --noRestore --noPackageCache`:

| Workload | Compile median ms, baseline → candidate | Process wall median ms | Allocation median MB |
| --- | --- | --- | --- |
| Binding | 1,794 → 1,811 (+0.9%) | 3,494 → 3,486 | 1,220 → 1,219 |
| JS suite | 2,176 → 2,273 (+4.5%) | 4,185 → 4,342 | 7,813 → 7,812 |

Every run produced byte-identical JS (67 binding files and 141 JS-suite files).
[Raw results](benchmarks/core-runtime/aligned-map-comparison.csv) include round zero warmups.
The hints did not establish a useful gain and were removed. No production source, vendored
binary or packaged runtime was changed. These are IL experiments, not R2R release timings.
The next Core lead should come from allocation/call profiles of this aligned runtime;
plugin compatibility and broader Core tests remain gates before adopting a replacement.

### Aligned Core allocation and call profiles (2026-10-22)

Two fresh processes per workload were traced with sampled thread time and GC allocation ticks.
The experimental CLI/FCS IL baseline is the same as the map comparison above (its executable
banner still reports 0.5.1); this is not the packaged 0.5.3 R2R executable. Its loaded Core file
is byte-identical to the verified aligned baseline, SHA-256
`0d62ab9aed739975537e55caa4536a9e8fc7a2fcf11d9e6cb5135e23cfda5b9e`.

The analyzer selects stacks containing CheckExpressions, CheckDeclarations or
PostInferenceChecks. CPU summaries count only `CPU_TIME`, exclude unmanaged/wait samples,
and exclude Core frames above the innermost FCS frame so compiler callbacks are not charged
to collection or async wrappers. These are sampled sums across threads, not elapsed time or
predicted speedups. Allocation ticks estimate allocated bytes by sampled type and stack;
they are not exact object counts or retained heap sizes. All four traces report zero lost
events; each JS trace has one allocation tick without a stack, excluded from checking totals.

| Lead | Binding, two runs | JS suite, two runs | Interpretation |
| --- | --- | --- | --- |
| Entity-name table map-node allocations | 166.4 / 166.0 MiB | 3.2 / 4.1 MiB | Rebuilding persistent name tables in FCS dominates this binding-specific allocation lead. |
| `TypeToken` boxing under structural array hashing/equality | 2.5 / 1.8 MiB | 143.9 / 145.9 MiB | A repeatable Core/FCS boundary cost for the JS workload. |
| `MapTree.mk` sampled self time | 135 / 153 ms | 187 / 161 ms | Allocation/construction is visible; the previously rejected inlining hints did not solve it. |
| Generic arbitrary-array hashing/equality sampled inclusive time | not a leading cost | 514 / 706 ms | Includes runtime work below Core; the two operations' totals are added here, not nested Core helper totals. |

Total estimated checking-stack allocation was 1,080 / 1,081 MiB for binding and
3,909 / 3,903 MiB for JS. The final profiled outputs match the preceding baseline outputs
byte for byte: 67 binding JS files and 141 JS-suite files. No production changes were made.

The most focused next experiment is typed structural hashing/equality for FCS's struct
`TypeToken[]` boundary. `Array.GetValue` boxes its elements today. Replacing it with ordinary
.NET equality would not establish equivalence: retain F# element equality semantics, hash
combination and truncation, element traversal order, and any supplied comparer behavior.
A general Core array fast path must additionally preserve nonzero lower bounds, ranks,
null handling, NaN ER/PER behavior, short-circuiting, and exception order. Characterize these
before implementing a candidate. The entity-name map lead is primarily an FCS construction
strategy investigation; mutating existing map nodes would violate persistent-map semantics.

Saved [profile summaries](benchmarks/core-runtime/profile-results.json), collection driver
`notes/benchmarks/core-runtime/profile.ps1`, CPU analyzer and allocation parser make the
investigation repeatable. The CPU analyzer's synthetic check verifies callback exclusion,
wait exclusion, checking scope and self-time accounting. Raw traces stay in `temp/core-profile`.
The driver attaches by PID: launching the compiler under dotnet-trace stalled its MSBuild
children in this environment, so that abandoned run was stopped and is not included.

### TypeToken-array experiment — partially explored, paused (2026-10-22)

The experiment specialized only the FCS type-subsumption cache comparer. The initial
wrapper preserved the ordinary structural comparer's hash results and equality over
23,785 checks (788 keys, 47 token values), including all token cases, Stable/Unstable tags,
both coercion flags, null arrays, and lengths around the 19-element hash prefix. All ten
diagnostic/attribute snapshots matched; all benchmark runs produced identical JS (67/141 files).
Missing-candidate and deliberately corrupted-hash checks failed as intended.

Five measured runs per variant after warmups gave allocation medians of 1,219 → 1,218 MB
for binding and 7,800 → 7,721 MB for JS. **No speedup is established.** Background language
servers were active, and binding compile times alone varied from about 2 to 10 seconds.
[Raw wrapper results](benchmarks/type-token-arrays/wrapper-comparison.csv) are retained for
allocation/output evidence, not reliable timing comparison.

Source review found that the wrapper could leave generated hashing on Core's arbitrary-array
path. A direct typed comparer was drafted to avoid that path and cache-key boxing, but its
build was stopped when the user paused this frontier. It is **unvalidated**, saved only as
an experiment patch. The original source and local FCS output DLL were restored; neither
vendored binaries nor packages changed.

The expanded harness additionally checks all boundary-array pairs and default-key exception
behavior; it passed 62,594 checks against the initial wrapper. A default TTypeCacheKey with null structures throws NullReferenceException when
hashed by both the baseline and initial wrapper. The unvalidated direct draft's null handling
would return a hash instead: this is an observable behavior difference to resolve before any
future adoption, even though normal cache-key construction does not produce that value.

Resume from `notes/benchmarks/type-token-arrays/`: characterize exception behavior first,
validate a corrected direct comparer against the live structural oracle, then rerun allocation
and end-to-end measurements under quieter conditions. Broader compiler/plugin and R2R
validation remain outstanding. Do not treat either saved patch as release-ready.

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
| Package-like sources outside `fable_modules` | done | JS/TS: `paket-files` sources compiled ahead of the project's own files are cached with the package sources; their path relative to the project and whether there is an out dir are in the key, their output is stored apart (`<entry>/outside`), and they are left out when another source dir would map to the same out dir. Needed the anonymous record fix below. Ionide compile phase 2.77 → 2.15 s; output identical but for one local named after a cached member's parameter (`uri` for `options`). Python and BEAM not enabled: no `paket-files` project to check them on, and BEAM names modules from source paths. |
| Anonymous record types across the cached assembly | done | fickle-fsharp `c0e7f8143`. An anonymous record type belongs to its assembly, so a cached API taking an anonymous record no longer accepted the project's own (Ionide: "Two anonymous record types are from different assemblies"), and an overload taking one could silently lose to an `obj` overload. This affected the existing package cache too, for any package with such an API. `InteractiveChecker.SetAnonRecdTypesUnifiedAssembly` makes the project's and the cache's assemblies one for anonymous records (other assemblies unchanged), set on a hit. Edge cases checked against a source compile: an `obj`/anonymous overload pair, a struct anonymous record, a cached function returning one, an annotated binding and a copy-and-update. |
| Cache build blocks the watcher on a miss | open | ~3.5 s once per package set and options; could run in the background. |
| Files waiting on `Promise.fs` at the start of checking | explained | Fable.Promise's `PromiseImpl.fs` is a root-level `[<AutoOpen>] module` that calls `Promise.PromiseBuilder()`, so the graph correctly makes every later file depend on it and, through it, on `Promise.fs`. A package cache hit removes it: in Ionide checking starts at 0 ms instead of 1.0 s and the check wall goes 2.55 → 2.19 s; the critical path is then `Fable.Import.VSCode.fs` (1.07 s, from `paket-files`). |
| Fable.Promise check cost | open | 0.6–1.1 s per project; find what in it is slow to check. |
| IWSAM-as-type check collecting extension methods | done | fickle-fsharp `594a55f8b`. `CheckIWSAM` runs for every interface named in a signature and gathered all methods in scope, extension methods included, only to drop extension members: 44% of checking Glutinum's 18.7k-line `TypeScript.fs` (stock `fsc` spends the same 3.0 s on it). Now intrinsic methods only; same warnings. `TypeScript.fs` 3.2–3.5 → 1.9–2.0 s, Glutinum compile 4.4–5.0 → 3.4 s, Ionide 3.0–3.5 → 2.5 s, Sutil −0.2 s, JS suite unchanged; output identical. Upstreamable to dotnet/fsharp. |
| Large binding files: what remains | open | After the fix, `TypeScript.fs`'s profile is IL metadata reads for imported generic parameters (~12%), attribute checking (~12%), post-inference checks (~10%). |
| Codegen witness allocation | done | `7687bf609`. |
| Parallel cold type check over the file graph | done | `ac0a7b8c5`. Gain depends on how wide the project's graph is. |
| Symbol API caching | done | `7d83faa66`. |
| Watch mode: recheck only dependents | done | `fff58bb81`. |
| Watch mode: skip dependents when the edited file's signature is unchanged | done | fickle-fsharp `667b537e2`. Early cutoff: a changed file is checked again, and when a fingerprint of its signature (types with typars by position, constraints, attributes and their arguments, literal values, inline flags, argument names, accessibility, members and slots, unions, fields, exceptions; ranges and stamps left out; anything else makes it count as changed) equals the earlier one, its earlier finisher stays in the state, so dependents keep consistent entity identities and are reused, and only the new implementation is reported. JS suite, body edit in `Util.Testing.fs`: 57 files rechecked → 0, compile 1089 → 20 ms, edit to done 1315 → 249 ms (median of 6). Watch output matched a fresh compile in 29 scripted edits (`.fsi` pairs, SRTP, measures, abbreviations, AutoOpen, literals, CompiledName, inline bodies, anonymous records, type errors, inferred-only dependencies). Also fixes a 0.4.0 bug: when Fable checked part of the project and then all of it in one cycle, a dependent of a file the first check replaced was reused against the old result in the second (stale errors and output); results now record the generation of each dependency's finisher. `FICKL_FCS_SIGNATURE_CUTOFF=0` turns the cutoff off. |
| Critical path of the type check (deep chains, largest file) | open | Measure per project. |
| Type checker hot paths (name resolution, constraint solving) | open | Needs a profile on an external project. |
| Assembly import (`TcImports`, IL and pickled signature reading) | open | Grows with package count. |
| Persisted type check results across processes | open, large | Today every new process re-checks every file even when Fable's cache is warm. |
| `arrayPathEq` / entity-ref equality | open | Span-based compare. |
| Remaining allocation (`FSharpExprConvert`, type hashing) | open | 2.9 GB in `FSharpExprConvert` before the symbol caches. |
| Parser static-constructor contention | ruled out | At most ~0.1 s. |

### Binding-file follow-up (2026-10-22, Windows)

An isolated project checks Glutinum CLI's unchanged 18,721-line `src/Glutinum.Converter/TypeScript.fs`
at `acfa66e39c79d6a608e030b5f60fd6536596e190`, with Fable.Core 5.4.0 and Glutinum.Types 1.0.0-beta.2.
Local fixtures, scripts and raw measurements are under ignored `temp/binding-probe/`.

Installed Fickl 0.5.1, `--noCache --noPackageCache`: the net10.0 fixture's first unprofiled run spent
1,476 ms in parse/check, 27 ms in Fable per-file work, and allocated 1,719 MB during compilation.
Attaching dotnet-trace by PID (avoiding inherited diagnostic-port suspension in MSBuild) gave these
inclusive sampled checking-stack shares. These are single profiles, not speedup measurements:

| Fixture target | CheckIWSAM | Attribute checking | Post-inference checks |
| --- | --- | --- | --- |
| net10.0 | 22.1% | 15.0% | 13.3% |
| netstandard2.1, matching Glutinum | 21.3% | 16.1% | 16.9% |

Generic-parameter metadata reading was not prominent in either isolated profile. This does not invalidate
the earlier full-project profile: the isolated project has fewer references and a different checking context.
The strongest remaining leads here are repeated intrinsic interface-member traversal and attribute checking.

Dictionary insertion/locking also appeared hot, so two independent changes were tried against FCS
`667b537e2`. These comparisons used matching **IL** CLI builds, not the installed composite R2R package.
Five alternating runs per variant after warm-up, `--noCache --noRestore --noPackageCache`; binding fixture
targets net10.0. Values below are median compilation wall milliseconds (excluding cracking):

| Experiment | Binding baseline → candidate | JS suite baseline → candidate | Allocation change |
| --- | --- | --- | --- |
| Call-local PropertyCollector: Dictionary instead of ConcurrentDictionary | 1,979 → 1,995 | 2,325 → 2,257 | Binding −7 MB; JS −36 MB |
| TranslateTopArgSynInfo: allocate/store only on a cache miss | 2,016 → 1,975 | 2,187 → 2,327 | Binding −1 MB; JS −2 MB |

Neither was retained: the first did not improve the target binding workload, and the second's small binding
improvement did not carry across to the JS suite. Run-to-run spreads also caution against treating these
small differences as reliable wins. Every run produced byte-identical JS against its baseline (67 files
including the runtime for the binding fixture, 141 for the JS suite). This is output-equivalence evidence,
not a full compiler conformance test.

Build/validation limitations: a fresh FCS build failed in the netstandard2.0 dependencies with FS0074
(`System.Array` unavailable). Reusing existing dependency binaries and building FCS with
`BuildProjectReferences=false; ProduceReferenceAssembly=false` succeeded for baseline and candidates;
all three builds emitted the same four existing FS3261 symbol-API nullness warnings. The prebuilt compiler
test runner was stale relative to current source: its baseline failed `CaseTest in a TypeScriptTaggedUnion
does not emit warning`, `InlineInterface on a case without an interface field emits error`, and
`InlineInterface on a case with an interface field does not emit error`, referring to obsolete namespace
`Fickle` instead of current `Fickl.Core`. No passing-suite claim is made from that runner.

### Static-abstract interface warning fast path (2026-10-22)

The binding profile's `CheckIWSAM` subtree spent about half its samples filtering overrides and another
17% excluding hidden methods. Ordinary interfaces cannot trigger this warning, but previously paid for
those signature comparisons at every checked use. `InfoReader.HasIntrinsicStaticAbstractMethod` now
first scans the raw intrinsic method sets for a static dispatch slot. If none exists it returns false;
otherwise it runs exactly the previous override/hiding filters. It introduces no cache or invalidation rule.

Source changes are in the sibling `fickle-fsharp` checkout (`fickle_slim`, commit `c32d52e92`), in
`InfoReader.fs`, `InfoReader.fsi`, and `Expressions/CheckExpressions.fs`. The rebuilt FCS DLL and XML are
vendored in `lib/fcs`. The dependency-build workaround described above was still required.

Five alternating measured runs after warm-up, with cache and package cache disabled:

| Build / workload | Median compile ms, baseline → candidate | Median allocation MB |
| --- | --- | --- |
| IL / binding | 1,963 → 1,773 (−9.7%) | 1,715 → 1,219 (−28.9%) |
| IL / JS suite | 2,197 → 2,250 (+2.4%; variable) | 7,815 → 7,811 |
| Composite R2R / binding | 1,467 → 1,242 (−15.3%) | 1,719 → 1,225 (−28.7%) |
| Composite R2R / JS suite | 1,933 → 1,979 (+2.4%; variable) | 7,819 → 7,819 |

R2R binding process wall time fell 2,853 → 2,634 ms (−7.7%). JS-suite R2R paired compile deltas were
−8, −18, +108, +10, and −23 ms: these measurements support a binding-heavy workload improvement, not a
general compiler speedup. Every measured run had byte-identical JS to its baseline (68 binding output
files, including runtime and an existing helper, and 141 JS-suite files). The R2R outputs were regenerated
after clearing stale crossgen intermediates; the initial accidentally identical publishes were discarded.
Raw results: `temp/binding-probe/iwsam-il-comparison.csv` and `iwsam-r2r-comparison.csv`.

The fresh compiler test runner passed all 140 tests, including seven warning cases: ordinary inherited
generic interfaces, repeated static-abstract uses, inherited slots, concrete static members, recursive
declarations, imported `IParsable`, and a concrete static member hiding an inherited slot. Baseline and
candidate diagnostic snapshots for these cases were identical. The previous stale runner failures above
do not recur after rebuilding. Static review found no actionable issue; diagnostic timing/location for
malformed imported metadata and type-provider callbacks have not been dynamically tested.

The generated JS-suite output passed all 3,162 Node tests. The broader `test integration` command
stopped after 41/42 integration tests passed: `pathMap` expects a forward slash in `CallerFilePath`,
but Windows emits a backslash. The same focused test fails identically with the baseline FCS DLL.
No snapshot was changed. The command therefore did not reach its later project-configuration tests;
the separate 140-test compiler run above passed with the candidate DLL.

### Next investigation: attribute constructor checking (2026-10-22)

Watch latency remains deferred. Two fresh profiles of the unchanged binding fixture using the
validated 0.5.3 win-x64 package confirm attribute checking as the next focused investigation.
Both runs used `--noCache --noRestore --noPackageCache --timings` and attached dotnet-trace by PID.
Profiled parse/check wall times were 1,470 and 1,557 ms, with 1,223 MB allocated in each run.
These instrumented runs establish a profile, not an optimization benchmark.

Attribute checking accounted for 18.0–18.1% of sampled checking stacks, post-inference checks
19.7–21.0%, and `CheckIWSAM` 6.8–8.0%. Within attribute checking, `TcMethodApplication`
accounted for 62–66% and `ResolveObjectConstructorPrim` 21–27%. Stable sorting appeared in
19–23% of attribute samples, with array cloning underneath it. These inclusive shares overlap.

Next: trace constructor-group sorting and overload resolution to determine whether repeated
work can be avoided while preserving overload order, accessibility checks, and diagnostics.
Test any candidate against overloaded and named attribute arguments, then compare alternating
binding and JS-suite runs with output equivalence. No compiler change or speedup is claimed yet.
Cold project loading and process startup remain subsequent candidates; the previously rejected
dictionary experiments are not being repeated solely because locks still appear in profiles.

Local traces and analysis are in ignored `temp/binding-probe/attributes-053*.speedscope.json`,
`profile.ps1`, and `attribute-profile.py`. Cleanup verified no leftover MSBuild workers after
the runs, excluding active Rider evaluations.

#### Two-constructor stable-sort experiment: not retained

`Item.MakeCtorGroup` sorts by total argument count before resolution. The candidate specialized
exactly two constructors: evaluate the first key and then the second once each, swap only for
`firstCount > secondCount`, otherwise preserve the original order. All other group sizes used
the existing stable sort. This preserved tie order, accessibility filtering, and key-evaluation
order; it added no cache. Removing sorting or using an unstable sort would change the intent
and was not attempted. Method groups were left unchanged.

Matched Release IL FCS builds from `c32d52e92`, five interleaved measured runs plus warmups,
`--noCache --noRestore --noPackageCache` ([raw results](benchmarks/attribute-constructors/comparison.csv)):

| Workload | Compile median ms, baseline → candidate | Process wall median ms | Allocation median MB |
| --- | --- | --- | --- |
| Binding | 2,220 → 2,158 (−2.8%) | 4,354 → 4,434 | 1,219 → 1,219 |
| JS suite | 2,443 → 2,514 (+2.9%) | 4,725 → 4,973 | 7,806 → 7,809 |

Binding baseline compile times ranged 1,943–3,327 ms and candidate 1,938–2,314 ms.
The results do not establish a useful improvement, so the source change was removed and no
vendored compiler binary was replaced. Every measured run produced byte-identical JS to its
baseline (68 binding files, 141 JS-suite files). These were IL experiments, not release-package
R2R measurements; no claim is made about an R2R speedup.

The saved characterization harness in `notes/benchmarks/attribute-constructors` covers ascending
and descending constructor arity, equal-arity overloads, named properties, private constructors,
bad arguments and arity, ambiguity, three constructors, and imported attributes. It compares
complete diagnostic snapshots (including duplicate errors, order, text and ranges), plus emitted
attribute constructor and named arguments from the checked assembly signature. All ten cases
matched for baseline and candidate. A deliberately altered expected diagnostic signature was
rejected, verifying the snapshot comparison is active. This is not a compiler mutation test or
exhaustive proof of semantic equivalence. No full compiler suite was run for this rejected candidate.

Run the saved harness against the vendored baseline:

```powershell
rtk dotnet run --project notes/benchmarks/attribute-constructors -c Release -- temp/attribute-observed.txt notes/benchmarks/attribute-constructors/baseline.txt
```

Both FCS builds used `BUILDING_USING_DOTNET=true`, `BuildProjectReferences=false`, and
`ProduceReferenceAssembly=false`; both reported the same four existing FS3261 nullness warnings.
The next investigation should distinguish actual repeated constructor-resolution work from
sorting samples before expanding the optimization scope. Watch latency remains deferred.

#### Constructor-resolution instrumentation: cache not justified

Temporary instrumentation of `ResolveObjectConstructorPrim` measured lookup, default-constructor
handling/accessibility filtering, and constructor-group creation separately. Three fresh IL
processes checked the binding fixture after one uninstrumented baseline run. Each recorded
4,591 successful constructor groups, comprising 3,757 distinct `(type name, source file, line)`
locations. Every recorded type was imported (`IsLocalRef = false`). Line-based locations may
combine different attributes on the same line; they are not proof of identical checking context.

| Instrumented run | Lookup ms | Filtering/default handling ms | Group creation/sort ms |
| --- | --- | --- | --- |
| 1 | 35.156 | 34.863 | 9.110 |
| 2 | 10.514 | 13.262 | 6.376 |
| 3 | 11.453 | 13.964 | 15.867 |

The repeated bulk work is small: 910 `AllowNullLiteralAttribute` lookups cost 0.6–1.2 ms
per run, and 749 `EmitAttribute` lookups cost 0.4–1.2 ms. `AbstractClassAttribute` accounts for
a large part of the first run's lookup/filter time despite only 12 calls. Timers include
first-use/JIT, GC and scheduling effects; stderr logging is outside the timed sections but
still perturbs execution. These measurements are diagnostic evidence, not speedup predictions.
They do not support adding a constructor cache or treating the earlier sampling share as
removable work. The measurement covers successful ordinary constructor-group construction;
delegate and no-constructor early returns are not included.

The repeated type-attribute checks have documented intent. `TcTyconDefnCore_Phase1B_EstablishBasicKind`
first checks against broad `TyconDecl` targets to infer the type kind, then rechecks against the
actual class/struct/interface/enum target under `EnforceAttributeTargets`. Separately,
`TcAttributesCanFail` retries failures after members in a recursive group become available.
Skipping repeated source locations could therefore lose target errors, change duplicate-error
behavior, or retain an early failed lookup. Caching raw imported constructors would still need
to preserve source-range-specific metadata diagnostics, accessibility and type instantiations;
the measured benefit does not justify that added complexity.

All 67 JS files in a fresh output directory were byte-identical between the baseline and all
three instrumented runs (the earlier 68-file directory contained an existing extra helper).
The ten saved attribute cases also matched full diagnostics and checked attribute arguments.
Instrumentation was removed from compiler source, and the local build output DLL was restored
to the matching uninstrumented baseline. No vendored binary was changed.

[Measurements](benchmarks/constructor-resolution/measurements.json) and the
[temporary instrumentation patch](benchmarks/constructor-resolution/instrumentation.patch)
are saved for audit. Timers use `Stopwatch.GetTimestamp`; frequency was verified as 10,000,000 Hz.
Local driver and raw stderr are under ignored `temp/binding-probe/ctor-resolution/`.
This closes constructor lookup/caching as a low-priority lead on this fixture; it does not rule
out other workloads or the broader argument-checking and post-inference costs.

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
| Project cracking on a cold cache | partial | 2–5 s per first run (MSBuild design-time build); cached afterwards. The main project's TargetFramework query was a `dotnet msbuild` process ahead of the design-time build; when the project file states one unconditional TargetFramework the two now run side by side, and the design-time build reruns with the queried value if they differ (checked: a Directory.Build.targets override, multi-targeting, a conditional TargetFramework). Ionide 7.6 → 3.9 s, Fantomas tools and Sutil −0.5 s. References were cracked after the main build, level by level, one process each; the references a project file states directly (unconditional, no properties, one stated TargetFramework, up to one per core) now start beside the main build, and a result is used when the referencing build asks for the same TargetFramework and restore rewrote none of the assets and `nuget.g` files it read (`FICKL_CRACK_SPECULATION=0` turns it off). JS suite cracking 13.4 → 8.2 s, median of 5 interleaved, output identical; checked: cold restore (rejected, recracked), a Directory.Build.targets TargetFramework override (rejected), multi-targeting and conditional references (not speculated), `--exclude`. Remaining: restore on every cold run. |
| .NET CLI telemetry in the compiler's own `dotnet` processes | done | Each `dotnet msbuild` (and the `dotnet build` of an `--exclude`d project) sent telemetry on exit and waited for it: here about 9 s per process (on a VPN), against 0.6 s with `DOTNET_CLI_TELEMETRY_OPTOUT=1`. Those processes now get it unless the user set the variable; user `--run` commands are unchanged. JS suite load 27.4 → 1.9 s, wall 31.9 → 5.8 s, output identical. Upstream pays it too. |
| Watch mode fixed overhead | open | ~230–300 ms rebuilds on small edits; find what the floor is made of. |
| Process startup and assembly loading | open | Measure. |
