/// Fickle: opt-in phase timings for a compilation, enabled with --timings.
/// Fable compiles files while FCS is still type-checking the project, so phases overlap:
/// per-file work is summed across threads and reported next to the wall-clock totals.
module Fable.Cli.Timings

open System
open System.Diagnostics
open System.Threading
open Fable.Compiler.Util

let mutable private enabled = false

let setEnabled value = enabled <- value

// Counter cells, kept in an array so Interlocked can update them from compiling threads
[<Literal>]
let private CrackMs = 0

[<Literal>]
let private CheckMs = 1

[<Literal>]
let private FileTicks = 2

[<Literal>]
let private MaxFileTicks = 3

[<Literal>]
let private FileCount = 4

[<Literal>]
let private AllocatedAtStart = 5

let private counters: int64[] = Array.zeroCreate 6
let mutable private gcAtStart = [| 0; 0; 0 |]

let private gcCounts () =
    [| GC.CollectionCount 0; GC.CollectionCount 1; GC.CollectionCount 2 |]

/// Clears the per-compilation counters; project cracking is kept, it only happens once.
let reset () =
    for i in CheckMs..FileCount do
        Interlocked.Exchange(&counters[i], 0L) |> ignore

    counters[AllocatedAtStart] <- GC.GetTotalAllocatedBytes false
    gcAtStart <- gcCounts ()

let recordCrack (ms: int64) =
    if enabled then
        counters[CrackMs] <- ms

/// Times the whole-project FCS parse and type check.
let timeCheck (work: Async<'a>) : Async<'a> =
    if not enabled then
        work
    else
        async {
            let sw = Stopwatch.StartNew()
            let! res = work
            Interlocked.Add(&counters[CheckMs], sw.ElapsedMilliseconds) |> ignore
            return res
        }

let rec private updateMax (ticks: int64) =
    let current = Volatile.Read(&counters[MaxFileTicks])

    if
        ticks > current
        && Interlocked.CompareExchange(&counters[MaxFileTicks], ticks, current) <> current
    then
        updateMax ticks

/// Times one file through FSharp2Fable, FableTransforms, the target transform, printing and writing.
let timeFile (work: Async<'a>) : Async<'a> =
    if not enabled then
        work
    else
        async {
            let sw = Stopwatch.StartNew()
            let! res = work
            let ticks = sw.Elapsed.Ticks
            Interlocked.Add(&counters[FileTicks], ticks) |> ignore
            Interlocked.Increment(&counters[FileCount]) |> ignore
            updateMax ticks
            return res
        }

let private ticksToMs (ticks: int64) = TimeSpan(ticks).TotalMilliseconds

/// Prints the timings table for a compilation that took totalMs of wall-clock time.
let report (totalMs: int64) =
    if enabled then
        let allocatedMb =
            float (GC.GetTotalAllocatedBytes false - counters[AllocatedAtStart])
            / (1024.0 * 1024.0)

        let gcs = Array.map2 (-) (gcCounts ()) gcAtStart
        let fileCount = counters[FileCount]
        let filesMs = ticksToMs counters[FileTicks]

        let avgMs =
            if fileCount = 0L then
                0.0
            else
                filesMs / float fileCount

        let maxMs = ticksToMs counters[MaxFileTicks]
        let nl = Environment.NewLine

        Log.always (
            $"{nl}Timings (--timings):{nl}"
            + $"  Project cracking (wall)          %8d{counters[CrackMs]} ms{nl}"
            + $"  FCS parse + type check (wall)    %8d{counters[CheckMs]} ms{nl}"
            + $"  Fable per-file work (summed)     %8.0f{filesMs} ms over %d{fileCount} files (avg %.1f{avgMs} ms, max %.1f{maxMs} ms){nl}"
            + $"  Compilation total (wall)         %8d{totalMs} ms{nl}"
            + $"  Allocated                        %8.0f{allocatedMb} MB{nl}"
            + $"  GC collections gen0/gen1/gen2    %d{gcs[0]}/%d{gcs[1]}/%d{gcs[2]}{nl}"
        )
