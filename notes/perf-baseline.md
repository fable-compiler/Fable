# Performance baseline

Measured with `./fickle-build.sh timings` (the `--timings` report), Release compiler, cold process, `--noCache`.

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

### Next measurements

- dotTrace sampling + allocation snapshot of a cold run, split by FCS phase (parse, import, type check).
- The same table for Python and for a large external project.
