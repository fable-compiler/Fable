# Fable.Beam — F# on the BEAM

An Erlang/BEAM target for Fable.

## Motivation

- Learn the BEAM/OTP platform deeply by building a compiler target (the same
  approach that made Fable.Python a success for learning Python)
- Bring F#'s type system, pattern matching, and computation expressions to the
  BEAM ecosystem
- Bring F#'s `MailboxProcessor` to the BEAM as an in-process, source-compatible
  abstraction (so existing F# async/agent code just compiles) — note this is a
  same-process CPS model, **not** an OTP actor; `MailboxProcessor` turned out not to be
  the right surface for real process-isolated OTP actors
- Provide real OTP concurrency (process-isolated actors, supervision trees, hot code
  reloading) through the separate [Fable.Beam](https://github.com/fable-compiler/Fable.Beam)
  bindings library and the [Fable.Actor](https://github.com/fable-hub/Fable.Actor) model,
  rather than overloading `MailboxProcessor`

## Minimum OTP Version: 25

Fable.Beam requires **OTP 25 or later**. This is the oldest version still available
via `apt install erlang` on current Linux LTS distributions (Ubuntu 22.04, Debian 12,
Pop!_OS). Users should be able to install Erlang from their distro package manager
and have it work without adding third-party repos.

Key OTP features the runtime depends on:

| Feature | Minimum OTP | Used In |
| --- | --- | --- |
| Maps (`#{}`) | 17 | All modules |
| Named funs (`fun F(X) -> ...`) | 17 | Generated recursive lambdas |
| `erlang:monotonic_time/1` | 18 | `fable_stopwatch.erl` |
| `erlang:system_time/1` | 18 | `fable_date.erl` |
| `atomics` module | 21 | `fable_utils.erl` (byte arrays), `fable_random.erl` |
| `uri_string` module | 21 | `fable_uri.erl` |
| JIT compiler | 24 (improved in 25) | Bit-syntax integer wrapping performance |
| `maybe` keyword reserved | 25 | Escaped in `sanitizeErlangName` |

## Target Language: Erlang Source

Generate `.erl` files (not Core Erlang, not Elixir). Rationale:

- **Learning-first**: reading generated Erlang output teaches the language
- **OTP integration**: OTP docs and patterns are written in Erlang
- **Debuggable**: users can read and understand the output
- **Sufficient**: Erlang surface syntax is regular enough for code generation
- Can always add a Core Erlang backend later if performance demands it

## Architecture

Same pipeline as all Fable targets:

```text
F# Source
    ↓  FSharp2Fable (existing)
Fable AST
    ↓  FableTransforms (existing)
Fable AST (optimized)
    ↓  Fable2Beam
Erlang AST
    ↓  ErlangPrinter
.erl source files
```

## Design Principles: Beam as an Independent Target

Fable.Beam should be its own target with its own idioms. While the compiler pipeline
is shared with other targets, the **Replacements layer and runtime library should
take full advantage of Erlang/BEAM capabilities** rather than inheriting patterns
from JavaScript or Python that don't fit.

### Don't inherit JS/Python patterns when Erlang is simpler

Many .NET BCL operations that require complex library emulation in JS or Python map
directly to Erlang built-ins:

| Area | JS/Python approach | Erlang approach |
| --- | --- | --- |
| **Integers** | Fixed-width emulation (JS BigInt, Python PyO3 Rust) | Native arbitrary-precision — direct `+`, `-`, `*`, `div`, `rem` |
| **Int64/BigInt** | Library calls (`big_int:op_add`, etc.) | Direct binary ops — Erlang integers ARE arbitrary-precision |
| **Bitwise ops** | JS routes Int64 through BigInt library | Native `band`, `bor`, `bxor`, `bsl`, `bsr`, `bnot` |
| **Lists** | Array-based emulation | Native linked lists — direct `[H\|T]`, `lists:*` |
| **Maps** | Library objects/dicts | Native `#{}` maps, `maps:*` |
| **Sets** | Library Set class | Native `ordsets` (sorted lists) |
| **Structural equality** | `Util.equals()` library call | Native `=:=` (deep comparison on all types) |
| **Structural comparison** | `Util.compare()` library call | Native `<`, `>`, `=<`, `>=` (works on all terms) |
| **Hashing** | Custom hash functions | `erlang:phash2/1` |
| **Pattern matching** | Compiled to if/else chains | Native pattern matching in `case` expressions |
| **Sequences** | Lazy iterators | Lazy seqs via compiled `seq.erl`/`seq2.erl` |

**Rule: If Erlang can do it natively, do it natively.** Only create library modules
(`fable-library-beam/*.erl`) for operations that genuinely need helper code. Avoid
falling through to the JS Replacements fallback for Beam-specific operations.

### Never modify F# tests to accommodate Erlang quirks

The F# test suite represents valid F# code that must compile and run correctly on all
targets. When an Erlang edge case causes a test failure:

- **DO**: Add a helper function in `fable-library-beam/` that handles the edge case
  (e.g., `fable_convert:to_float/1` handles `"1."` which `binary_to_float/1` rejects)
- **DO**: Use `#if FABLE_COMPILER` blocks for genuine cross-platform differences
  (e.g., .NET CultureInfo in parsing)
- **DON'T**: Change the F# test input to avoid the edge case (e.g., changing
  `float("1.")` to `float("1.0")` — this hides the bug)

### Intercept in Beam Replacements, not JS fallback

The Replacements pipeline tries `Beam.Replacements.tryCall` first, then falls back to
`JS.Replacements.tryCall`. The JS fallback injects extra arguments (comparers, adders)
that Erlang doesn't need and generates library imports (`Util`, `BigInt`, etc.) that
don't exist in Erlang.

**Handle operations in Beam Replacements** to get clean, original argument lists.
Reserve the JS fallback only for operations that genuinely work the same way.

## New Files

### Compiler transforms (`src/Fable.Transforms/Beam/`)

| File                       | Purpose                                             | Reference                     | Status |
| -------------------------- | --------------------------------------------------- | ----------------------------- | ------ |
| `Beam.AST.fs`              | Erlang AST type definitions (intentionally minimal) | `Python/Python.AST.fs`        | Done   |
| `Fable2Beam.fs`            | Main Fable AST → Erlang AST transforms              | `Fable2Php.fs` / Python       | Done   |
| `Fable2Beam.Util.fs`       | Shared helpers for the transforms                   | `Python/Fable2Python.Util.fs` | Done   |
| `Fable2Beam.Reflection.fs` | Compile-time reflection type-info generation        | Python reflection             | Done   |
| `Replacements.fs`          | .NET BCL → Erlang mappings (full implementation)    | `Python/Replacements.fs`      | Done   |
| `ErlangPrinter.fs`         | Erlang AST → `.erl` source code                     | `Python/PythonPrinter.fs`     | Done   |
| `Prelude.fs`               | Name sanitization + Erlang keyword escaping         | —                             | Done   |

Started as a single `Fable2Beam.fs` (PHP pattern) and has since split out
`Fable2Beam.Util.fs` and `Fable2Beam.Reflection.fs` as complexity grew (Python pattern).
The Erlang AST (`Beam.AST.fs`) deliberately stayed small — see "Erlang AST" below.

### Runtime library (`src/fable-library-beam/`)

Erlang modules implementing F# core types:

| Module | Purpose | Notes | Status |
| --- | --- | --- | --- |
| fable_option.erl | Option | Some(x) = x, None = undefined | Done |
| fable_list.erl | FSharpList | fold, find, choose, collect, etc. | Done |
| fable_map.erl | FSharpMap | Erlang native maps, pick/try_pick/min/max | Done |
| fable_seq.erl | Seq / IEnumerable | Eager lists, delay/singleton/unfold | Done |
| fable_string.erl | String utilities | Erlang binaries, pad/replace/join, sprintf/printf/String.Format | Done |
| fable_comparison.erl | Comparison | compare/2 returning -1/0/1 | Done |
| fable_char.erl | Char utilities | is_letter/digit/upper/lower/whitespace | Done |
| fable_convert.erl | Type conversions | Robust to_float handling edge cases | Done |
| fable_reflection.erl | Reflection | Full FSharpType/FSharpValue support: TypeInfo as maps, record/union/tuple/function type tests, GetRecordFields/MakeRecord, GetUnionFields/MakeUnion, GetTupleFields/MakeTuple, PropertyInfo.GetValue | Done |
| fable_result.erl | Result | {ok, V} or {error, E} | Done |
| fable_set.erl | FSharpSet | ordsets (sorted lists), fold/map/filter/partition/union_many/intersect_many | Done |
| fable_async_builder.erl | AsyncBuilder | CPS builder operations (bind, return, delay, etc.) | Done |
| fable_async.erl | Async | High-level ops (RunSynchronously, Parallel, Sleep, etc.) | Done |
| fable_regex.erl | Regex | Wraps Erlang `re` module (PCRE), IsMatch/Match/Matches/Replace/Split | Done |
| fable_resize_array.erl | ResizeArray | List manipulation helpers (set_item, remove, insert, find, sort) | Done |
| fable_dictionary.erl | Dictionary | Mutable dictionary via process dict + Erlang maps, TryGetValue with out-refs | Done |
| fable_hashset.erl | HashSet | Mutable set via process dict + Erlang maps, UnionWith/IntersectWith/ExceptWith | Done |
| fable_queue.erl | Queue | FIFO queue via process dict + Erlang queue module | Done |
| fable_stack.erl | Stack | LIFO stack via process dict + list | Done |
| fable_timespan.erl | TimeSpan | Ticks-based, create/from_*/component accessors/total_*/arithmetic/parse/to_string | Done |
| fable_date.erl | DateTime | 2-tuple {Ticks, Kind}, calendar module, formatting, parsing, arithmetic | Done |
| fable_date_offset.erl | DateTimeOffset | 3-tuple {Ticks, OffsetTicks, Kind}, wraps fable_date | Done |
| fable_guid.erl | Guid | UUID v4 generation, parse, toString, comparison | Done |
| fable_uri.erl | Uri | URI parsing and manipulation | Done |
| fable_utils.erl | Utilities | IEnumerator (lists/refs/maps/HashSet), apply_curried, infinity/NaN helpers, atomics byte arrays (new_byte_array, byte_array_get/set/length) | Done |
| fable_bit_converter.erl | BitConverter | Byte conversion, endianness | Done |
| fable_decimal.erl | Decimal | Fixed-scale integer (value × 10^28), multiply/divide/to_string/parse/from_parts | Done |
| fable_mailbox.erl | MailboxProcessor | In-process CPS continuation model (same as JS/Python) | Done |
| fable_cancellation.erl | CancellationToken | Process dict pattern, create/cancel/register/is_cancellation_requested, timer-based auto-cancel | Done |
| fable_stopwatch.erl | Stopwatch | StartNew, Elapsed, ElapsedMilliseconds, Stop, Reset, Restart, IsRunning, Frequency, GetTimestamp | Done |
| fable_observable.erl | Observable | subscribe, add, choose, filter, map, merge, pairwise, partition, scan, split | Done |
| fable_event.erl | Event / IEvent | trigger/publish/add, choose/filter/map/merge/pairwise/partition/scan/split | Done |
| fable_date_only.erl | DateOnly | create/components/day_number/add_*/from_date_time/to_string/parse | Done |
| fable_time_only.erl | TimeOnly | create/from_*/components/ticks/add_*/is_between/to_time_span/to_string/parse | Done |
| fable_parallel.erl | Array.Parallel | spawn-based parallel_map/mapi/init/iter/iteri/collect/choose/for | Done |
| fable_quotation.erl | F# Quotations (Expr) | `mk_*` constructors, `is_*` tests, evaluate, substitute, get_free_vars | Done |

### Registration & CLI (modified existing files) -- All Done

|                      File                      |                     Change                      | Status |
| ---------------------------------------------- | ----------------------------------------------- | ------ |
| `src/Fable.AST/Plugins.fs`                     | Added `Beam` to `Language` DU                   | Done   |
| `src/Fable.Compiler/Util.fs`                   | Added `.erl` file extension                     | Done   |
| `src/Fable.Compiler/ProjectCracker.fs`         | Added Beam library path                         | Done   |
| `src/Fable.Cli/Entry.fs`                       | Added `--lang beam` argument parsing            | Done   |
| `src/Fable.Cli/Pipeline.fs`                    | Added `Beam.compileFile` dispatch               | Done   |
| `src/Fable.Transforms/Replacements.Api.fs`     | Beam dispatch for all 15 API functions          | Done   |
| `src/Fable.Transforms/Transforms.Util.fs`      | Added Beam to `getLibPath`                      | Done   |
| `src/Fable.Transforms/FSharp2Fable.Util.fs`    | Added Beam to `isModuleValueCompiledAsFunction` | Done   |
| `src/Fable.Transforms/Fable.Transforms.fsproj` | Added Beam files to project                     | Done   |

### Build system (`src/Fable.Build/`)

|          File          |                      Change                      | Status |
| ---------------------- | ------------------------------------------------ | ------ |
| `FableLibrary/Beam.fs` | `BuildFableLibraryBeam` class                    | Done   |
| `Quicktest/Beam.fs`    | Quicktest handler for beam                       | Done   |
| `Test/Beam.fs`         | Test handler (`./build.sh test beam`)            | Done   |
| `Main.fs`              | Added beam to quicktest + test + fable-library   | Done   |
| `Fable.Build.fsproj`   | Added new Beam files                             | Done   |

### Quicktest project (`src/quicktest-beam/`)

|         File          |               Purpose               | Status |
| --------------------- | ----------------------------------- | ------ |
| `quicktest.fs`        | `printfn "Hello from BEAM!"`        | Done   |
| `quicktest.fsproj`    | Project file referencing Fable.Core | Done   |

## Type Mappings

### Natural fits (F# → Erlang)

|        F#        |         Erlang         |                   Notes                    |                              |
| ---------------- | ---------------------- | ------------------------------------------ | ---------------------------- |
| `int`, `float`   | `integer()`, `float()` | Direct                                     |                              |
| `string`         | `binary()`             | `<<"hello">>`                              |                              |
| `bool`           | `true \                | false`                                     | Atoms                        |
| `unit`           | `ok`                   | Atom                                       |                              |
| `tuple`          | `tuple`                | Direct: `{A, B, C}`                        |                              |
| `list<T>`        | `list()`               | Both are linked lists!                     |                              |
| `option<T>`      | `x` or `{some, x}` \   | `undefined`                                | Erased or wrapped (see below)|
| `Result<T,E>`    | `{ok, V} \             | {error, E}`                                | Matches Erlang idiom exactly |
| Pattern matching | Pattern matching       | Both languages excel here                  |                              |
| Immutability     | Immutability           | Erlang is immutable by default             |                              |
| `bigint`         | `integer()`            | Erlang has native arbitrary-precision ints |                              |

### Requires design decisions

|                F#                 |                  Erlang Strategy                  |             Alternatives             |
| --------------------------------- | ------------------------------------------------- | ------------------------------------ |
| **DU cases**                      | Tagged tuples: `{some, V}`, `{node, L, R}`        | Maps, records                        |
| **Records**                       | Erlang maps: `#{name => <<"Dag">>}`               | Erlang records (compile-time tuples) |
| **Classes**                       | Module + map (state as map, methods as functions) | Processes holding state              |
| **Interfaces**                    | Dispatch maps: `#{method => fun(...) -> ... end}` | Done (object expressions)            |
| **Mutability** (`ref`, `mutable`) | Process dictionary, ETS, or process state         | Agent pattern                        |
| **Exceptions**                    | `throw`/`catch` with tagged tuples                | Error tuples (Erlang way)            |
| **Generics**                      | Erased (Erlang is dynamically typed)              | —                                    |
| **Currying**                      | Lambda wrapping (same as Python target)           | —                                    |
| **Nested modules**                | Flat module names: `My_Module_Sub`                | One file per module                  |
| **Computation expressions**       | Transformed at Fable AST level; async/task → CPS  | —                                    |

## Concurrency & Async

This compiler/runtime implements F#'s async and agent model **in-process** (CPS-based),
keeping mutable process-dict state reachable. Real OTP process concurrency (`gen_server`,
supervision, distribution) is layered on top via the separate
[Fable.Beam](https://github.com/fable-compiler/Fable.Beam) bindings — see
"OTP Processes, Supervision & Actors" below.

### MailboxProcessor — In-Process CPS Model (IMPLEMENTED)

F#'s `MailboxProcessor` uses same-process CPS continuations (matching JS/Python targets),
NOT OTP gen_server. This design was chosen because:

1. F# MailboxProcessor body closures capture mutable state from the caller — a separate
   Erlang process can't access process dict state from the parent
2. The CPS async framework (`run_synchronously`) uses process dict `put`/`erase`,
   requiring same-process execution
3. gen_server would require compile-time extraction of message handlers from an opaque
   async body — extremely complex for no semantic benefit

The implementation mirrors `src/fable-library-py/fable_library/mailbox_processor.py`.

| F# | Erlang |
| --- | --- |
| `new MailboxProcessor(body)` | `fable_mailbox:default(Body)` — creates agent with empty queue |
| `MailboxProcessor.Start(body)` | `fable_mailbox:start(Body)` — create + start |
| `agent.Start()` | `fable_mailbox:start_instance(Agent)` — run body via `start_immediate` |
| `inbox.Receive()` | `fable_mailbox:receive_msg(Agent)` — returns `Async<Msg>` via `from_continuations` |
| `agent.Post(msg)` | `fable_mailbox:post(Agent, Msg)` — queue + `process_events` |
| `agent.PostAndAsyncReply(f)` | `fable_mailbox:post_and_async_reply(Agent, F)` — reply channel + `Async<Reply>` |
| `replyChannel.Reply(v)` | `(maps:get(reply, Channel))(V)` — emitExpr inline |

**OTP actors live in Fable.Beam**: Real process-isolated actors (`gen_server` /
`gen_statem`, supervision, fault tolerance) are provided by the separate
[Fable.Beam](https://github.com/fable-compiler/Fable.Beam) OTP bindings and the
[Fable.Actor](https://github.com/fable-hub/Fable.Actor) library — not this in-process
MailboxProcessor.

### Async & Task — In-Process CPS (IMPLEMENTED)

`Async<T>` compiles to a **continuation-passing-style (CPS) function**, not an Erlang
process:

```erlang
Async<T> = fun(Ctx) -> ok end
Ctx      = #{on_success, on_error, on_cancel, cancel_token}
```

The function is *cold* — it does nothing until invoked with a context, matching F#'s
cold-async semantics. Composition (`bind`, `return`, `try/with`, `while`, `for`, …) just
threads new contexts through these functions; see `fable_async_builder.erl`.

**Everything runs inline in the caller's process.** This is deliberate, not a
limitation: the CPS body reaches mutable state stored in the process dictionary (mutable
`let`, ref cells, arrays, MailboxProcessor queues), and running in the same process keeps
that state reachable. `RunSynchronously`, `StartImmediate`, and `StartWithContinuations`
all execute the chain in the current process — no `spawn`, no trampoline (Erlang has
native TCO).

| F# | Erlang (`fable_async` / `fable_async_builder`) |
| --- | --- |
| `async { return x }` | `fun(Ctx) -> (maps:get(on_success, Ctx))(X) end` |
| `let! x = comp` / `do!` | `bind(Comp, fun(X) -> ... end)` — CPS monadic bind |
| `return` / `return!` | `return/1` / `return_from/1` |
| `try/with`, `try/finally` | `try_with/2`, `try_finally/2` (CPS `on_error`/compensation + Erlang try/catch) |
| `while` / `for` | `while/2` / `for/2` (recursive bind) |
| `Async.StartImmediate` | `start_immediate/1` — inline, default context (fire-and-forget) |
| `Async.RunSynchronously` | `run_synchronously/1` — inline, result stashed via a process-dict ref |
| `Async.StartWithContinuations` | `start_with_continuations/4` — inline with caller continuations |
| `Async.Sleep` | `timer:sleep(Ms)` inline; with a cancel token, `receive` waits on a timer/cancel message (still same process) |
| `Async.Sequential` | run each computation inline via `run_synchronously` |
| `Async.Catch` | `catch_async/1` — wraps result in `{choice1_of2,_}` / `{choice2_of2,_}` |
| `Async.Ignore` | `bind` + `return ok` |
| `Async.FromContinuations` | `from_continuations/1` — lower-level CPS primitive |
| `task { ... }` | alias for async (Task is an alias for Async on Beam) |
| `task.Result` | `fable_async:run_synchronously(Comp)` |

**The one exception — `Async.Parallel` spawns.** To get real parallelism it `spawn`s one
process per child computation and collects results in order via message passing
(`fable_async:parallel/1`). Each child runs `run_synchronously` in its own process, so
parallel children do **not** share the parent's process-dict state. This is the only
place the async runtime leaves the caller's process.

Cancellation (`fable_cancellation.erl`) is also in-process: a token is a `make_ref()`
keying a process-dict map `#{cancelled, listeners, next_id}`. `Async.Sleep` is the only
operation that observes it cooperatively (via the `receive` path above).

### OTP Processes, Supervision & Actors — Provided by Fable.Beam (separate library)

Real BEAM concurrency — spawning processes, `gen_server`, `supervisor`, supervision
trees, applications, ETS, distribution — is **not** part of this compiler or runtime
library. It lives in the separate
[Fable.Beam](https://github.com/fable-compiler/Fable.Beam) bindings library, which provides
typed F# bindings to OTP modules (`Fable.Beam.GenServer`, `Fable.Beam.Supervisor`,
`Fable.Beam.Application`, `Fable.Beam.Erlang`, `Fable.Beam.Ets`, …) plus an actor model
([Fable.Actor](https://github.com/fable-hub/Fable.Actor)).

The compiler's job is only to emit correct Erlang; OTP behaviours are opt-in F# bindings
layered on top. Speculative OTP API design (attributes, `supervisor { }` CEs, direct
`gen_server` interop, etc.) belongs in the Fable.Beam repo, not here.

## Implementation Phases

### Phase 1: Hello World -- COMPLETE

Get the full pipeline working end-to-end with minimal features.

- [x] Add `Language.Beam` to the DU
- [x] Minimal Erlang AST (module, function, expression, literal)
- [x] Minimal Fable2Beam (constants, `printfn`, simple functions)
- [x] Minimal ErlangPrinter (output valid `.erl`)
- [x] CLI integration (`--lang beam` / `--lang erlang`)
- [x] Compile and run: `printfn "Hello from BEAM!"`

**Goal**: `dotnet fable --lang beam` produces a `.erl` file that `erlc` compiles and runs.

**What works now**: String/int/float/bool literals, tuples, lists, `printfn`, let bindings,
sequential expressions, type casts, curried apply, emit expressions. Unhandled Fable
expressions produce `todo_*` atom placeholders. The `printfn` chain goes through
`printf` → `toConsole` → `io:format`.

**How to test**:

```bash
dotnet build src/Fable.Cli
dotnet run --project src/Fable.Cli --no-launch-profile -- \
  --cwd src/quicktest-beam src/quicktest-beam/quicktest.fsproj \
  --lang beam --outDir /tmp/beam-out --noCache
```

**Note**: Phase 1 quicktest command above is for manual exploration. For the full
automated test suite, use `./build.sh test beam` (see Phase 2).

### Phase 2: Core Language -- COMPLETE

Core F# language features that map naturally to Erlang. All implemented in
`Fable2Beam.fs` with corresponding AST additions and printer updates.

- [x] Arithmetic operators (`+`, `-`, `*`, `div`/`/`, `rem`) with int/float distinction
- [x] Comparison operators (`=:=`, `=/=`, `<`, `=<`, `>`, `>=`)
- [x] Bitwise operators (`band`, `bor`, `bxor`, `bsl`, `bsr`, `bnot`)
- [x] Logical operators (`andalso`, `orelse`, `not`)
- [x] Exponentiation via `math:pow/2`
- [x] If/else → `case Guard of true -> Then; false -> Else end`
- [x] Lambda (single arg, curried) → `fun(Arg) -> Body end`
- [x] Delegate (multi-arg, uncurried) → `fun(A, B) -> Body end`
- [x] CurriedApply → `Apply` node for calling fun values
- [x] Test expressions (`UnionCaseTest`, `ListTest`, `OptionTest`)
- [x] Get expressions (`TupleIndex`, `UnionTag`, `UnionField`, `ListHead`, `ListTail`, `OptionValue`, `FieldGet`, `ExprGet`)
- [x] DecisionTree / DecisionTreeSuccess (following JS target pattern)
- [x] NewList fix → `ListCons` (`[H | T]` instead of `[H, T]`)
- [x] NewUnion → atom-tagged tuples `{atom_tag, Field1, Field2, ...}`, bare atoms for fieldless cases
- [x] NewOption → value or `undefined` atom
- [x] Set expressions (ValueSet → variable rebind, FieldSet → `maps:put`)
- [x] LetRec → sequential fun assignments
- [x] AST additions: `ListCons`, `Apply` on `ErlExpr`; `PList` on `ErlPattern`
- [x] Printer: `BinOp` parenthesization, `UnaryOp` word operator spacing

**What works now**: Most basic F# programs compile to valid Erlang. Operators,
conditionals, functions (named, anonymous, higher-order, partial application),
pattern matching (integers, strings, booleans, DUs, options, lists, tuples),
decision trees, and let/letrec bindings all produce correct Erlang output.

**Test suite**: `tests/Beam/` with xUnit. Run with `./build.sh test beam` which:

1. Runs all tests on .NET via `dotnet test`
2. Compiles tests to `.erl` via Fable (library files auto-copied to `fable_modules/fable-library-beam/`)
3. Compiles library `.erl` files in `fable_modules/fable-library-beam/` with `erlc`
4. Compiles test `.erl` files with `erlc -pa fable_modules/fable-library-beam`
5. Runs an Erlang test runner (`erl_test_runner.erl`) with `-pa fable_modules/fable-library-beam` that discovers and executes all `test_`-prefixed functions

The Erlang test runner discovers and runs every `test_`-prefixed arity-1 function. The
suite currently has **2446 passing tests across 63 test files** — more than the Python
target. Coverage spans the F# core library and language features:

- **Collections**: Seq, List, Array (incl. byte arrays via `atomics`), Map, Set,
  ResizeArray, Dictionary, HashSet, Queue, Stack
- **Primitives & text**: arithmetic (incl. Int64, BigInt, decimal), bitwise/logical/
  comparison, string, char, regex, conversions, encoding
- **Types**: records, unions, tuples, anonymous records, enums, classes/interfaces,
  object expressions, units of measure, structural equality/comparison, reflection
- **Date/time**: DateTime, DateTimeOffset, DateOnly, TimeOnly, TimeSpan, Guid, Uri,
  Stopwatch
- **Control flow & effects**: pattern matching, active patterns, loops, exceptions,
  type testing, tail calls, async/task, MailboxProcessor, cancellation, observables/events
- **Interop**: `emitErl`, `Import`, `ImportAll` + `Erase` interfaces, module calls
- **Integration**: a Sudoku solver and a raytracer demo

See `tests/Beam/` for the individual test files.

### Phase 3: Discriminated Unions & Records -- COMPLETE

F#'s defining feature on BEAM. DU basics (construction and pattern matching via
DecisionTree) were implemented in Phase 2. This phase adds records and structural equality.

- [x] DU declaration → tagged tuple constructors (Phase 2)
- [x] DU pattern matching → clause matching on tagged tuples (Phase 2)
- [x] Records → Erlang maps (`#{field => value}`)
- [x] Record update syntax → F# compiler decomposes into `NewRecord` + `FieldGet` (works automatically)
- [x] Anonymous records → Erlang maps (field names provided inline)
- [x] Structural equality for DUs and records → Erlang's native `=:=` (deep comparison)

**Design decisions**:

- Records map to Erlang maps (`#{name => <<"Alice">>, age => 30}`)
- Field names converted to snake_case atoms
- `FieldGet` → `maps:get(field, Map)`, `FieldSet` → `maps:put(field, Value, Map)`
- Structural equality uses Erlang's native `=:=` operator (deep comparison for all
  types: tuples, maps, lists, atoms, numbers, binaries) — no runtime library needed.
  Implemented via `Beam/Replacements.fs` intercepting `GenericEquality`/`op_Equality`
  before JS Replacements generates `Util.equals` library calls.

### Phase 4: Collections -- COMPLETE

- [x] `list<T>` → Erlang lists (cons cells — natural fit)
- [x] List module functions → `lists:` module calls + `fable_list.erl` library
- [x] `array<T>` → process dict refs wrapping Erlang lists (mutable via `put`/`get`); byte arrays use `atomics` for O(1) read/write
- [x] `Map<K,V>` → Erlang native `#{}` maps, `maps:` module calls + `fable_map.erl`
- [x] `Set<T>` → Erlang `ordsets` (sorted lists), `ordsets:` module calls + `fable_set.erl`
- [x] `Seq<T>` → eager Erlang lists with `fable_seq.erl` library
- [x] `fable-library-beam` runtime: `fable_list.erl`, `fable_map.erl`, `fable_string.erl`, `fable_option.erl`, `fable_seq.erl`
- [x] Range expressions: `[1..n]` → `lists:seq(1, n)`, `[1..2..n]` → `lists:seq(1, n, 2)`
- [x] Array indexing: `arr.[i]` → `lists:nth(i + 1, arr)` (0-based to 1-based)
- [x] Array comprehensions: `[| for i in 0..n -> expr |]` via Seq desugaring

**Design decisions**:

- Sequences use **lazy evaluation** via Fable-compiled `seq.erl`/`seq2.erl` modules
  (compiled from `Seq.fs`/`Seq2.fs`). List-backed operations delegate to `fable_list.erl` BIFs.
- Seq operations intercepted in Beam Replacements (not JS fallback) to avoid
  injected comparers/adders that Erlang doesn't need.
- Complex operations in `fable_list.erl`/`fable_seq.erl`, simple BIF mappings via `emitExpr`.
- Scalar Seq operations (Fold, Reduce, Find, etc.) routed through compiled `seq.erl`
  (Fable-compiled from `Seq.fs`) via `Helper.LibCall` with `SignatureArgTypes`. This enables
  the `uncurrySendingArgs` FableTransform to automatically convert curried callbacks to
  uncurried Delegates, matching the pattern used by the Python target.
- BIF qualification: `ErlangPrinter.fs` automatically prefixes known BIFs (`length`, `hd`,
  `tl`, `element`, `put`, `get`, etc.) with `erlang:` in `Call(None, ...)` nodes. This
  prevents shadowing when compiled library modules (like `seq.erl`) define functions with
  the same name as BIFs.
- Integration tested with a Sudoku solver (SudokuTests.fs) using Seq, Array, ranges, and
  array comprehensions.
- Sets use Erlang's `ordsets` module (sorted lists). Maintains ordering compatible with
  F#'s structural comparison. Simple operations (`add`, `contains`, `union`, etc.) map
  directly to `ordsets:*` BIFs. Higher-order operations (`fold`, `map`, `filter`) use
  `fable_set.erl` for curried function handling. Set `+`/`-` operators intercepted in
  Beam `operators` via `Builtin(FSharpSet _)` arg type matching. `set [1;2;3]` handled
  via `CreateSet` → `ordsets:from_list`.

### Phase 5: Modules & Imports -- PARTIALLY COMPLETE

- [x] F# modules → Erlang modules (one `.erl` per module)
- [ ] Nested modules → flattened names or separate files
- [x] Module function calls → `module:function(args)` syntax
- [x] Import resolution and path handling
- [x] Export lists (`-export([...])`)
- [x] Snake_case output filenames (matching Erlang module name convention)
- [x] Function name sanitization (`$XXXX` hex sequences from F# backtick names)
- [x] Cross-module call resolution (derive module from `importInfo.Path`)
- [x] Inline `assertEqual`/`assertNotEqual` assertions (no util dependency needed)
- [x] `fable_modules/fable-library-beam/` output structure (aligned with JS/Dart/Rust targets)

### Phase 6: Error Handling -- COMPLETE

- [x] try/with → try/catch with `erlang:error` for exceptions
- [x] `failwith` → `erlang:error(<<"message">>)`
- [x] Exception message access via `#{message => Reason}` map wrapping
- [x] Nested try/catch works
- [x] `Result<T,E>` integration with Erlang `{ok,V}/{error,E}` convention
- [x] Custom F# exception types (`exception MyError of string`) → maps with `exn_type` tag
- [x] Exception type discrimination in catch: `maps:get(exn_type, X, undefined) =:= type_name`
- [x] Multi-field exceptions: `exception MyError2 of code: int * message: string`
- [x] Exception `.Message` property via `message` field in exception map

### Phase 6b: Types & Type Testing -- COMPLETE

Extend type system support for common F# patterns.

- [x] **Enum support** — F# enums are just integers in Erlang (trivial, works out of box)
    - Enum declaration, construction, pattern matching — all native
    - `int` ↔ enum conversion, `enum<MyEnum>(n)` — TypeCast is erased
    - Enum comparison, flags (bitwise) — native Erlang operators
    - `EnumOfValue`/`EnumToValue` — TypeCast is erased
- [x] **Custom exceptions** — `exception MyError of string` → maps with `exn_type` atom tag
    - Exception construction via `NewRecord` adds `exn_type` and `message` fields
    - Pattern matching in try/catch: `maps:get(exn_type, X, undefined) =:= type_name`
    - Exception `.Message` property via `message` field in exception map
    - TryCatch handler preserves exception maps (is_map check), wraps non-maps in `#{message => ...}`
    - Multi-field exceptions with named fields work correctly
- [x] **Type testing (`:?`)** — runtime type checks via Erlang guards
    - `match x with :? int as i -> ...` → `is_integer(X)` guard
    - Primitive types: `is_binary` (string), `is_boolean` (bool), `is_float` (float), `is_integer` (int)
    - Collection types: `is_list` (list/array), `is_tuple` (tuple), `is_map` (record/class)
    - Exception types: `is_map(X) andalso maps:get(exn_type, X, undefined) =:= type_name`
    - `box`/`unbox` are erased (TypeCast)
- [x] **String interpolation fix** — `fable_string:to_string/1` for generic value formatting
    - Replaces `~p` format (which showed `<<"...">>` for binaries) with runtime type dispatch
    - Handles binary/integer/float/atom natively, falls back to `~p` for complex terms
- [x] **Curry expressions** — uses `Replacements.Api.curryExprAtRuntime` to generate nested lambdas at compile time (no runtime module needed)

### Phase 7: Async & Task -- COMPLETE

CPS (Continuation-Passing Style) implementation. `Async<T>` = `fun(Ctx) -> ok end` where
`Ctx = #{on_success, on_error, on_cancel, cancel_token}`. CPS naturally gives cold semantics
(F# Async is cold — doesn't execute until started). Task CE is an alias for Async on the Beam
target since Erlang has no equivalent of .NET's hot Task distinction.

- [x] `async { }` computation expression → CPS builder via `fable_async_builder.erl`
- [x] `let!` / `do!` → `bind/2` (monadic bind — run computation, pass result to binder)
- [x] `return` / `return!` → `return/1` / `return_from/1`
- [x] `try/with` in async → `try_with/2` (CPS on_error override + synchronous try/catch)
- [x] `while` / `for` in async → `while/2` / `for/2` (recursive bind)
- [x] `Async.RunSynchronously` → runs in same process (preserves process dict access)
- [x] `Async.StartImmediate` → runs with default context (fire-and-forget)
- [x] `Async.Parallel` → spawn one process per computation, collect via message passing
- [x] `Async.Sleep` → `timer:sleep/1`
- [x] `Async.Ignore` → bind + return unit
- [x] `Async.StartWithContinuations` → direct CPS invocation
- [x] `Async.FromContinuations` → lower-level CPS primitive
- [x] `task { }` computation expression → alias for async builder
- [x] `task.Result` → `fable_async:run_synchronously`
- [x] Cancellation tokens → process dict pattern with `fable_cancellation.erl`

**Design decisions**:

- **CPS over spawn**: `Async<T>` is a function `fun(Ctx) -> ok end`, not a spawned process.
  CPS naturally gives cold semantics matching F# Async. No trampoline needed — Erlang has
  native tail call optimization.
- **Everything runs inline in the caller's process** — `RunSynchronously`, `StartImmediate`,
  `StartWithContinuations`, and `Sequential` never `spawn`. The only exception is
  `Async.Parallel`, which spawns one process per child for real parallelism (children run
  `run_synchronously` in their own process and don't share the parent's process-dict state).
- **RunSynchronously runs in same process**: Uses a process-dict ref to store the result, NOT
  `spawn` + `receive`. This preserves mutable variable (process dict) access from the async body.
- **Task = Async alias**: Task CE builder methods route to `fable_async_builder`, Task
  instance methods (`.Result`, `.GetAwaiter().GetResult()`) route to `run_synchronously`.
- **try_with dual handler**: Both the CPS `on_error` override AND the `try/catch` must invoke
  the Handler — synchronous throws (like `erlang:error`) bypass the CPS on_error path.
- **Erlang function naming**: `return`, `for`, `while` are NOT reserved words in Erlang —
  they work as function names in remote calls (`fable_async_builder:return(V)`).

### Phase 8: MailboxProcessor -- COMPLETE

In-process CPS continuation model (same pattern as JS/Python targets). Uses process dict
for mutable state, `fable_async:from_continuations` for the receive/reply coordination.

- [x] `MailboxProcessor.Start` → `fable_mailbox:start(Body)` (create + start_immediate)
- [x] `new MailboxProcessor(body)` → `fable_mailbox:default(Body)` (create only)
- [x] `agent.Start()` → `fable_mailbox:start_instance(Agent)`
- [x] `inbox.Receive()` → `fable_mailbox:receive_msg(Agent)` (Async via from_continuations)
- [x] `agent.Post(msg)` → `fable_mailbox:post(Agent, Msg)` (queue + process_events)
- [x] `agent.PostAndAsyncReply(f)` → `fable_mailbox:post_and_async_reply(Agent, F)`
- [x] `replyChannel.Reply(v)` → `(maps:get(reply, Channel))(V)` (emitExpr inline)

**Design decisions**:

- **Same-process, not gen_server**: MailboxProcessor body closures capture mutable state via
  process dict. A separate process can't access this state. The CPS model runs everything
  inline in the caller's process, matching F# semantics exactly.
- **State as process dict map**: Agent = `#{ref => Ref}` where `Ref` keys into process dict
  storing `#{body, messages, continuation}`. Mutable queue + continuation slot.
- **Synchronous reply coordination**: `post_and_async_reply` stores a reply callback in the
  reply channel map. Since everything runs synchronously via CPS, by the time `post` returns
  the inbox has processed the message and called Reply, so the value is available immediately.
- **Named `receive_msg`**: Erlang's `receive` is a reserved keyword, so the function is named
  `receive_msg`. The Replacements dispatch maps `"Receive"` → `receive_msg`.
- **OTP actors live in Fable.Beam**: Process-isolated actors and supervision are provided by
  the separate [Fable.Beam](https://github.com/fable-compiler/Fable.Beam) OTP bindings and
  [Fable.Actor](https://github.com/fable-hub/Fable.Actor) — not this in-process MailboxProcessor.

### Phase 9: OTP Patterns — Moved to Fable.Beam

OTP integration (supervision trees, application behaviour, hot code reloading,
distribution / multi-node) is **out of scope for the compiler**. It is provided by the
separate [Fable.Beam](https://github.com/fable-compiler/Fable.Beam) bindings library
(`gen_server`, `supervisor`, `application`, `ets`, `erlang` process BIFs, …) and the
[Fable.Actor](https://github.com/fable-hub/Fable.Actor) actor model. The compiler only
needs to emit correct Erlang that those bindings can call.

### Phase 10: Ecosystem

- [x] Build integration: `rebar3` project generation — `Main.fs` lays out files under
  `src/` (rebar3 convention), generates the root `rebar.config`, and per-dependency
  `src/<app>.app.src` + `rebar.config`. Fable-generated configs are regenerated; user-owned
  ones are detected and left untouched.
- [x] Test suite (`tests/Beam/` — 2446 tests passing, `./build.sh test beam`)
- [x] Erlang test runner (`tests/Beam/erl_test_runner.erl` — discovers and runs all `test_`-prefixed arity-1 functions)
- [x] `erlc` compilation step in build pipeline (per-file with graceful failure)
- [x] Quicktest setup (`src/quicktest-beam/`, `Fable.Build/Quicktest/Beam.fs`)
- [ ] Documentation

## Erlang AST

`Beam.AST.fs` is deliberately small (~80 lines) and that proved sufficient for the entire
test suite. Operators are plain strings (not typed DUs), `if` lowers to `case`, and there
is no dedicated `ListComprehension` / `BinaryExpr` / `MapUpdate` node — those F#
constructs are expressed with the existing nodes plus `Emit` as a raw-Erlang escape hatch.
The actual node set: `ErlLiteral`; `ErlPattern` (`PVar`/`PLiteral`/`PTuple`/`PList`/
`PWildcard`); `ErlExpr` (literals, variables, tuples, lists/`ListCons`, maps, `Call`/
`Apply`, `Fun`/`NamedFun`, `Case`, `Match`, `Block`, `BinOp`/`UnaryOp`, `TryCatch`,
`Emit`, `Receive`); attributes; function defs; and modules.

The richer AST below was the *original* aspirational design. It was **never needed** and
is kept only as a reference for what a fuller Erlang AST could look like if a future
feature (e.g. native list comprehensions or bit-syntax literals) ever warrants it:

```fsharp
module rec Fable.AST.Beam

type Atom = Atom of string

type Literal =
    | Integer of int64
    | Float of float
    | StringLit of string    // binary literal <<"...">>
    | AtomLit of Atom
    | BoolLit of bool
    | NilLit                 // empty list []

type Pattern =
    | PVar of string
    | PLiteral of Literal
    | PTuple of Pattern list
    | PList of Pattern list * Pattern option  // [H|T] pattern
    | PCons of Pattern * Pattern
    | PWildcard              // _
    | PMap of (Pattern * Pattern) list

type Guard = Expression list  // guard sequences

type Expression =
    | Literal of Literal
    | Variable of string
    | Tuple of Expression list
    | List of Expression list * Expression option  // [H|T]
    | Map of (Expression * Expression) list
    | MapUpdate of Expression * (Expression * Expression) list
    | BinOp of BinaryOp * Expression * Expression
    | UnaryOp of UnaryOp * Expression
    | Call of module_: Expression option * func: Expression * args: Expression list
    | Fun of FunClause list               // fun(Args) -> Body end
    | Case of Expression * CaseClause list
    | If of IfClause list
    | Receive of CaseClause list * Timeout option
    | Try of body: Expression list * catch_: CatchClause list * after_: Expression list
    | Block of Expression list            // begin ... end
    | Match of Pattern * Expression       // Pattern = Expr
    | ListComprehension of Expression * Qualifier list
    | BinaryExpr of BinaryElement list    // <<"hello">>

and CaseClause = { Pattern: Pattern; Guard: Guard; Body: Expression list }
and IfClause = { Guard: Guard; Body: Expression list }
and CatchClause = { Class: Atom option; Pattern: Pattern; Guard: Guard; Body: Expression list }
and FunClause = { Patterns: Pattern list; Guard: Guard; Body: Expression list }
and Timeout = { Duration: Expression; Body: Expression list }

type BinaryOp = Add | Sub | Mul | Div | IntDiv | Rem | Band | Bor | Bxor | Bsl | Bsr | And | Or | Andalso | Orelse | Append | Subtract
type UnaryOp = Not | Bnot | UAdd | USub
type ComparisonOp = Eq | NotEq | Lt | LtE | Gt | GtE | ExactEq | ExactNotEq

type Qualifier =
    | Generator of Pattern * Expression        // X <- List
    | BinaryGenerator of Pattern * Expression  // <<X>> <= Binary
    | Filter of Expression

type BinaryElement = { Value: Expression; Size: Expression option; TypeSpecifiers: Atom list }

type Attribute =
    | Module of Atom
    | Export of (Atom * int) list          // function/arity pairs
    | Import of Atom * (Atom * int) list
    | Behaviour of Atom
    | TypeSpec of name: Atom * spec: string  // -spec
    | CustomAttr of Atom * Expression list

type FunctionDef =
    { Name: Atom
      Arity: int
      Clauses: FunClause list }

type Form =
    | Attribute of Attribute
    | Function of FunctionDef
    | Comment of string

type Module =
    { Name: Atom
      Forms: Form list }
```

## Sized & Signed Integer Semantics

> **Status: not yet implemented.** Erlang's native arbitrary-precision integers make
> `int`, `int64`, and `bigint` work out of the box, so the test suite passes today without
> fixed-width wrapping. True sized-integer overflow semantics (`int8`/`int16`/`int32` and
> unsigned wrapping) are **not** implemented yet — the rest of this section is the design
> plan for when they are. Strategy A (bit-syntax wrapping) remains the recommendation.

### The Problem

.NET has fixed-width integers (`int8`, `int16`, `int32`, `int64`, `uint8`...`uint64`)
with specific overflow/wrapping behavior. Erlang, like Python, has arbitrary-precision
integers — no overflow, no fixed bit width.

For Fable.Python this required reimplementing all sized integer types in **Rust via
PyO3** (`src/fable-library-py/src/ints.rs`, ~1200 lines) using `wrapping_add`,
`wrapping_sub`, `wrapping_mul`, etc. Plus typed arrays in Rust for the same reason.
This was a major effort.

### Erlang Advantage: Bit Syntax

Erlang has a feature Python lacks — **binary pattern matching with bit-level
type specifications**. This can express wrapping semantics in pure Erlang:

```erlang
%% Wrapping int32 arithmetic — pure Erlang, no NIF needed
-module(fable_int32).
-export([add/2, sub/2, mul/2, from_int/1]).

add(A, B) -> wrap32(A + B).
sub(A, B) -> wrap32(A - B).
mul(A, B) -> wrap32(A * B).
from_int(N) -> wrap32(N).

wrap32(N) ->
    <<V:32/signed-integer>> = <<N:32/signed-integer>>,
    V.

%% Same pattern for all widths:
%% wrap8(N)  -> <<V:8/signed-integer>>  = <<N:8/signed-integer>>,  V.
%% wrap16(N) -> <<V:16/signed-integer>> = <<N:16/signed-integer>>, V.
%% wrap64(N) -> <<V:64/signed-integer>> = <<N:64/signed-integer>>, V.
%% uwrap32(N)-> <<V:32/unsigned-integer>> = <<N:32/unsigned-integer>>, V.
```

How it works:

1. `<<N:32/signed-integer>>` — constructs a 32-bit binary, truncating to low 32 bits
2. `<<V:32/signed-integer>> =` — pattern-matches it back, interpreting as signed
3. Result: correct two's complement wrapping, same as .NET

The BEAM JIT compiler (OTP 24+) optimizes binary operations heavily — this is one
of Erlang's most performance-critical paths (telecom/protocol workloads).

### Strategy Options

| Strategy | Effort | Performance | Correctness |
| -------- | ------ | ----------- | ----------- |
| **A. Bit syntax wrapping** (pure Erlang) | Low | Good (JIT-optimized) | Exact |
| **B. Band + sign extension** (pure Erlang) | Low | Good | Exact |
| **C. Rust NIF** (like Python) | High | Best | Exact |
| **D. Wrap at boundaries only** | Low | Best | Risky |

**Recommendation: Strategy A (bit syntax)** for initial implementation.

- No NIF compilation needed — pure Erlang, trivial to deploy
- Correct by construction (bit truncation + signed reinterpretation)
- If profiling shows it's a bottleneck, can move to NIF later
- Much simpler than the Python/PyO3 approach (~50 lines vs ~1200 lines)

Strategy B alternative using `band`:

```erlang
%% int32 via bitwise masking
wrap32(N) ->
    R = N band 16#FFFFFFFF,
    case R >= 16#80000000 of
        true  -> R - 16#100000000;
        false -> R
    end.
```

Both A and B are pure Erlang. A is more idiomatic and arguably clearer.

### Wrapping in Generated Code

Two approaches for where to wrap:

**Wrap every operation** (safe, slower):

```erlang
%% F#: let x = a + b * c
X = fable_int32:add(A, fable_int32:mul(B, C))
```

**Wrap at assignment only** (faster, still correct for most code):

```erlang
%% F#: let x = a + b * c
X = fable_int32:wrap(A + B * C)
```

The second is valid when intermediate overflow doesn't cross the 64-bit boundary
(extremely rare in practice). Start with wrap-every-op for correctness, optimize
later with a compiler flag if needed.

### Parsing

.NET integer parsing (`Int32.Parse`, `Int32.TryParse`) with `NumberStyles` support
needs implementation. In Erlang:

```erlang
parse_int32(Str) ->
    N = binary_to_integer(Str),
    case N >= -2147483648 andalso N =< 2147483647 of
        true  -> {ok, N};
        false -> {error, overflow}
    end.

parse_int32(Str, 16) ->
    N = binary_to_integer(Str, 16),
    wrap32(N).
```

### What About Arrays?

For Python, arrays also required Rust/PyO3 typed storage (`Int32Array`,
`Float64Array`, etc.) because Python lists box every element.

Erlang is better here:

- **Erlang tuples** — fixed-size, O(1) element access via `element(Index, Tuple)`,
  but immutable (copy-on-update via `setelement/3`)
- **Erlang `array` module** — functional sparse arrays, O(log n) access, good for
  large mutable-style arrays
- **ETS tables** — true mutable storage, O(1) access, but heavier setup
- **`atomics` module** (OTP 21+) — mutable integer arrays in shared memory,
  excellent for `int32[]` / `int64[]` use cases

Suggested approach:

- Small/read-heavy arrays → **tuples** (fast reads, copies on write)
- General case → **`array` module** (functional updates, good enough perf)
- Hot-path mutable int arrays → **`atomics`** (true O(1) mutable access)
- No Rust NIF needed — Erlang's built-in options cover the use cases

### Summary: Much Simpler Than Python

| Concern | Python Solution | Erlang Solution |
| ------- | --------------- | --------------- |
| Sized integers | Rust/PyO3 (~1200 lines) | Bit syntax (~50 lines pure Erlang) |
| Integer parsing | Rust/PyO3 (~100 lines) | `binary_to_integer` + bounds check |
| Typed arrays | Rust/PyO3 typed storage | Tuples / `array` module / `atomics` |
| Deployment | Needs Rust toolchain + compilation | Pure Erlang, no external deps |

This is a significant advantage of targeting BEAM over Python. The bit syntax
alone eliminates the single hardest piece of the Fable.Python runtime.

## Decisions Made

- **Strings**: Erlang binaries (`<<"hello">>`) — modern convention, more efficient
- **Module naming**: Snake_case derived from filename (`MyModule.fs` → `my_module`)
- **Replacements strategy**: Beam has its own dispatch in `Replacements.Api.fs` with
  JS fallback (`Beam.Replacements.tryCall` → `JS.Replacements.tryCall` if `None`).
  Beam handles equality, comparison, numerics, collections, and conversions natively;
  only operations that genuinely work the same way fall through to JS. All 15
  `Replacements.Api.fs` functions now have explicit Beam dispatch — `error` returns
  the message directly (wrapped by `makeThrow`), `defaultof` returns type-appropriate
  zero values, and ref cell operations use the process dictionary (`make_ref` +
  `put`/`get`).
- **File structure**: Single `Fable2Beam.fs` for Phase 1 (PHP pattern), split later
  as complexity grows (Python pattern)
- **DU representation**: Atom-tagged tuples `{atom_tag, Field1, ...}` for cases with
  fields, bare atoms for fieldless cases. Tag names derived via `sanitizeErlangName`
  (snake_case). `UnionCaseTest` guards non-fieldless checks with `is_tuple` to handle
  mixed DUs. `UnionTag` uses `case is_atom(X)` to dispatch between bare atoms and tuples.
- **If/else**: Uses `case Guard of true -> ...; false -> ... end` rather than
  Erlang's limited `if` (which only supports guard expressions)
- **Division**: `div` for integer types, `/` for float — determined by Fable's
  type information at compile time
- **Comparison operators**: Erlang's exact equality (`=:=`, `=/=`) rather than
  structural (`==`, `/=`), matching F#'s value equality semantics
- **DecisionTree**: Follows the JS (Babel) target pattern — inline targets with
  Let bindings, producing nested case expressions
- **Test framework**: xUnit with `[<Fact>]` attributes (matching Python/Rust targets),
  conditional compilation via `Util.fs` for future BEAM-side test execution
- **Records**: Erlang maps (`#{field => value}`), field names as snake_case atoms.
  Field access via `maps:get/2`, field update via `maps:put/3`.
- **Structural equality**: Erlang's native `=:=` for all types (no runtime library).
  Intercepted in `Beam/Replacements.fs` before JS Replacements generates `Util.equals`
  library calls. Works because `=:=` does deep comparison on tuples, maps, and lists.
- **Function name sanitization**: `sanitizeErlangName` in `Prelude.fs` decodes
  `$XXXX` hex sequences from F# compiled names (e.g. `$0020` → space), strips
  apostrophes, converts to snake_case, collapses/trims underscores, and escapes
  Erlang reserved words (e.g. `maybe` → `maybe_`, `receive` → `receive_`). The
  keyword escaping uses `checkErlKeywords` against the full OTP 25+ keyword set.
  Example: `test$0020infix$0020add$0020can$0020be$0020generated` → `test_infix_add_can_be_generated`
- **Output filenames**: Snake_case, following the Python target pattern. Uses
  `Naming.applyCaseRule Core.CaseRules.SnakeCase` in `Pipeline.fs` Beam module.
  Erlang requires module name to match filename, so `ArithmeticTests.fs` →
  `arithmetic_tests.erl` with `-module(arithmetic_tests).`
- **Library output layout**: Aligned with JS/Dart/Rust targets — library files go to
  `fable_modules/fable-library-beam/` under the output directory, not mixed with compiled
  project files. `ProjectCracker.fs` uses non-empty `buildDir` to trigger the standard
  `copyDir` mechanism. `getOutPath` in `Main.fs` checks `Naming.isInFableModules` to preserve
  the subdirectory structure for library files while keeping project files flat in `outDir`.
  Erlang resolves modules via code path (`-pa fable_modules/fable-library-beam`) rather than
  hierarchical imports. Third-party project output structure:
  `output/my_module.erl` + `output/fable_modules/fable-library-beam/{fable_list,fable_string,seq,...}.erl`.
- **Inline assertions**: `assertEqual`/`assertNotEqual` (and their `Testing_equal`/
  `Testing_notEqual` variants) are inlined as `case Actual =:= Expected of true -> ok;
  false -> erlang:error({assert_equal, Expected, Actual}) end` — no runtime dependency
  on a util module.
- **Unit parameters**: Erlang unused variable warnings suppressed by prefixing unit
  parameters with `_` via `toErlangVar` in `Fable2Beam.fs`.
- **discardUnitArg / dropUnitCallArg**: Symmetric unit stripping matching JS/Python/Dart.
  `discardUnitArg` strips trailing unit parameters from function definitions (Lambda,
  Delegate, ObjectExpr members, MemberDeclaration, class methods/constructors).
  `dropUnitCallArg` strips the corresponding unit argument at call sites (in `transformCall`).
  Both sides must be stripped symmetrically so Erlang arity matching works. The old
  workaround of appending `Value(UnitConstant)` in `bclType` was removed.
- **Block hoisting**: When `Let` bindings produce `Block [Match(...); body]` expressions,
  these Blocks are invalid inside Erlang argument positions (Call, Apply, BinOp). The
  `extractBlock` / `hoistBlocksFromArgs` / `wrapWithHoisted` helpers extract leading
  assignments and hoist them before the enclosing expression. This fixes
  match-in-expression patterns like `match x with ... |> equal "abc"`.
- **Recursive lambdas**: Self-recursive `let rec f x = ... f (x-1) ...` inside function
  bodies generates Erlang named funs: `fun F(X) -> ... F(X-1) ... end` (OTP 17+).
  Detected via `containsIdentRef` which checks if a lambda body references its own
  binding ident. Mutual recursion (`let rec ... and ...`) is supported by bundling the
  group into a single named fun dispatched by atom tag (`MutualRecBindings`).
- **String interpolation**: `$"text: {value}"` generates
  `iolist_to_binary([<<"text: ">>, integer_to_binary(Value)])`. Integer values use
  `integer_to_binary/1`, string values pass through, other types use
  `io_lib:format("~p", [Value])`. Note: `io_lib:format` needs a charlist format string,
  not a binary — uses `binary_to_list(<<"~p">>)` to convert. String concatenation from
  Fable's Replacements (`string:concat`) is intercepted and replaced with
  `iolist_to_binary([A, B])` since `string:concat` returns charlists, not binaries.
  `to_string` conversion lives in `fable_string:to_string/1`.
- **Assert temp variables**: Complex expressions in assertEqual/assertNotEqual are stored
  in temp variables (`Assert_actual_N`, `Assert_expected_N`) to avoid duplicate
  evaluation and Erlang "unsafe variable" errors from variable bindings inside case
  branches that get duplicated in error messages.
- **Option representation**: `None` = `undefined` atom. Simple `Some(x)` is **erased**
  (just `x`). Nested options (`Option<Option<T>>`), `GenericParam`, and `Any` types use
  **wrapped** representation: `Some(x)` = `{some, x}`. This avoids ambiguity when
  `Some(None)` would otherwise be indistinguishable from `None`. Runtime smart constructor
  `fable_option:some/1` handles wrapping at generic call sites. Unlike JS/Python, `Unit`
  does NOT need wrapping because Erlang's `ok` atom is distinct from `undefined`.
- **Object expressions / Interfaces**: `{ new IFoo with member _.Bar(x) = ... }` compiles
  to an Erlang map of closures: `#{bar => fun(X) -> ... end}`. Property getters are stored
  as evaluated values (not closures) since call sites use `Get(obj, FieldGet(name))` →
  `maps:get(name, Obj)`. Interface method calls use `(maps:get(method, Obj))(Args)`.
  Detection: `transformCall`'s `Get(calleeExpr, FieldGet, _, _)` branch checks if
  `calleeExpr.Type` is a `DeclaredType` with `entity.IsInterface`. Self-referencing
  members (e.g., `x.Print()` inside another member) are not yet supported.
- **ImportAll + Erase interface**: `[<ImportAll("module")>]` + `[<Erase>]` interface pattern
  for typed FFI bindings. `myModule.someMethod(args)` → `module:some_method(Args)`.
  Detected in both `transformCall` (method calls) and `transformGet` (property access) by
  matching `calleeExpr` as `Import` with `Selector = "*"`. Emits direct Erlang remote calls
  instead of `fable_utils:iface_get` dispatch. Method names are converted via
  `sanitizeErlangName` (camelCase → snake_case). Same pattern as JS/Python but with `:` call
  syntax instead of attribute access.

- **Async/Task CE**: CPS (Continuation-Passing Style) implementation. `Async<T>` is a function
  `fun(Ctx) -> ok end` with context map `#{on_success, on_error, on_cancel, cancel_token}`.
  No trampoline needed (Erlang has native TCO). `RunSynchronously` runs in same process
  (not spawned) to preserve process dict access for mutable variables. `Parallel` spawns one
  process per computation and collects results via message passing. Task CE is an alias —
  Task builder methods route to `fable_async_builder`, `.Result` routes to `run_synchronously`.
  Replacements routing: `FSharpAsyncBuilder`/`AsyncActivation` → `asyncBuilder`;
  `FSharpAsync`/`AsyncPrimitives` → `asyncs`; `TaskBuilder`/`TaskBuilderBase` → `taskBuilder`;
  `Task`/`Task<T>` → `tasks`. `DefaultAsyncBuilder` in operators → `fable_async_builder:singleton`.
- **NewArray block hoisting**: `NewArray(ArrayValues ...)` uses `hoistBlocksFromArgs` +
  `wrapWithHoisted` to hoist Let bindings out of array literal positions, matching the
  pattern used for Call/Apply/BinOp arguments.
- **sprintf / printfn / String.Format**: Full F# format string support via `fable_string.erl`
  runtime. `printf/1` parses format strings (`%d`, `%s`, `%.2f`, `%g`, `%x`, etc.) into a
  continuation-based format object `#{input, cont}`. `to_text` (sprintf), `to_console`
  (printfn), `to_console_error` (eprintfn), `to_fail` (failwithf) apply continuations with
  appropriate handlers. Multi-arity overloads (`to_text/1..5`) handle Fable's inlined arg
  passing where `CurriedApply` flattens curried args into a single call. `format/2` handles
  .NET `String.Format("{0} {1}", args)` with positional placeholders. Replacements routing:
  `fsFormat` function handles `PrintfFormat.ctor` (→ `printf`), `PrintFormatToString`
  (→ `to_text`), `PrintFormatLine`/`PrintFormat` (→ `to_console`), etc. Dispatched from both
  `operators` (for `ExtraTopLevelOperators.sprintf`) and `tryCall` (for `PrintfModule`/
  `PrintfFormat` entities). The old `toConsole` → `io:format("~s~n")` hack in Fable2Beam.fs
  was removed.
- **MailboxProcessor**: In-process CPS continuation model, NOT gen_server. Agent state lives
  in process dict keyed by `make_ref()`: `#{body, messages, continuation}`. `receive_msg`
  uses `from_continuations` to store OnSuccess as pending continuation; `post` adds to queue
  and calls `process_events`; `post_and_async_reply` creates a reply channel map
  `#{reply => Fun}` where Fun stores the reply value in process dict, and the synchronous
  CPS execution guarantees the value is available when `post` returns. `Reply` is dispatched
  via `emitExpr` as `(maps:get(reply, $0))($1)`. Replacements route
  `FSharpMailboxProcessor` and `FSharpAsyncReplyChannel` to the `mailbox` handler.
- **CurriedApply**: Uses a simple `List.fold` applying args one at a time:
  `cleanArgs |> List.fold (fun fn arg -> Apply(fn, [arg])) cleanApplied`. This matches
  JS (Fable2Babel.fs) and Python (Fable2Python.Transforms.fs). Never merge CurriedApply
  args into Call nodes — that causes badarity errors when calling curried closures.
- **Erlang keyword escaping**: `sanitizeErlangName` pipes through `checkErlKeywords` at
  the end to append `_` suffix to Erlang reserved words (e.g. `maybe` → `maybe_`,
  `receive` → `receive_`). The `erlKeywords` set covers OTP 25+ keywords including
  `maybe` and `else`. This is needed because F# identifiers like `maybe` are valid but
  would generate Erlang syntax errors.
- **Arrays as process-dict refs**: All non-byte arrays are now process-dict refs
  (`make_ref()` + `put/get`), enabling cross-function mutation. `NewArray` wraps with
  `fable_utils:new_ref([...])`. `derefArr`/`wrapArr` helpers in Replacements convert
  between refs and plain lists for bulk operations. Binary comparison operators on arrays
  use `fable_comparison:compare(A, B) op 0`. TypeTest for arrays uses `is_reference`.
  When an array *literal* flows directly into an FFI/Emit binding that derefs its
  argument (e.g. `maps:from_list(erlang:get($0))`), the naive output is
  `...erlang:get(fable_utils:new_ref([...]))` — a pointless process-dict round-trip
  on an immutable literal (it also leaks an un-erased process-dict entry).
  `simplifyArrayRefDerefs` in `Fable2Beam.fs` cancels it on the Beam AST when
  building an `Emit` node: if an argument is an inline `new_ref(list)` and *every*
  occurrence of its `$N` placeholder in the macro template is deref-wrapped
  (`erlang:get($N)`/`get($N)`), the deref is dropped from the template and the
  underlying list is passed directly. Gating on the argument's AST shape (rather than
  parsing rendered Erlang) keeps it robust; a ref-typed *variable* (a bound/mutable
  array) keeps its deref.
- **Byte arrays via atomics**: Byte arrays (`Array<byte>`) use Erlang's `atomics` module
  for true O(1) mutable read/write. Represented as `{byte_array, Size, AtomicsRef}` tuples.
  Runtime helpers in `fable_utils.erl` handle both direct tuples and process-dict
  ref-wrapped byte arrays: `byte_array_get/2`, `byte_array_set/3`, `byte_array_length/1`.
  `Array.zeroCreate<byte>` uses `new_byte_array_zeroed` (atomics are zero by default).
  `Array.create n v` uses `new_byte_array_filled` (avoids intermediate list allocation).
  This enabled a 2048x2048 raytracer demo to run at ~40s on BEAM with no hand-patches.
- **Mutable variable ref erasure**: Process dict refs for mutable `let` bindings are erased
  at scope exit (`erase(Key)`) to prevent leaks. The `EraseMutableRefs` expression is emitted
  at the end of function bodies and appended to Block expressions containing mutable bindings.
- **Stray ok atom removal**: The ErlangPrinter strips bare `ok` atoms (F# unit values) from
  non-final positions in all body contexts: top-level functions, fun/named-fun clauses, case
  clauses, try/catch bodies, and block expressions. Matches both `Literal(AtomLit "ok")` and
  `Emit("ok", [])` forms (the latter from `System.Object..ctor`).
- **CancellationToken**: Process dict pattern with `make_ref()` storing
  `#{cancelled, listeners, next_id}`. Supports `create`, `cancel`, `cancel_after` (timer-based),
  `register`/`unregister`, `is_cancellation_requested` (drains cancel messages from mailbox).
  Sleep integration uses `receive` instead of `timer:sleep` when token is present.
- **Stopwatch**: Runtime `fable_stopwatch.erl` using `erlang:monotonic_time(microsecond)`.
  Supports `StartNew`, `Start`, `Stop`, `Reset`, `Restart`, `Elapsed`, `ElapsedMilliseconds`,
  `IsRunning`, `Frequency`, `GetTimestamp`.
- **FSharp.Reflection**: Full runtime support via `fable_reflection.erl` + compile-time type
  info generation in `Fable2Beam.Reflection.fs`. TypeInfo = Erlang map with `fullname`,
  `generics`, and optional `fields` (records) or `cases` (unions). PropertyInfo/CaseInfo are
  maps with `name`, `typ`, `tag`, `fields`. Reflection functions renamed to avoid Erlang BIF
  clashes: `is_tuple` → `is_tuple_type`, `is_function` → `is_function_type`. Union tag matching
  uses atom tags via `erl_tag` field (matching Beam's `{atom_tag, Field1, Field2}` representation). `MakeRecord`/
  `MakeUnion` handle both plain lists and process-dict ref arrays. `GetRecordFields` resolves
  concrete types through `TypeCast` wrappers via `getConcreteType` helper.
- **Async error wrapping**: `wrap_error/1` in `fable_async.erl` normalizes raw errors so
  `.Message` accessor works: raw binaries (from `failwith`) → `#{message => Bin}`, maps
  (from `raise (exn ...)`) pass through, refs pass through, everything else → formatted map.
  Used in `catch_async`, `try_with`, and `try_finally`. `catch_async` uses atom tags
  `{choice1_of2, V}` / `{choice2_of2, wrap_error(E)}` matching Beam's Choice union representation.
- **OperationCanceledException**: Added to the exception type pattern in Beam Replacements
  alongside `BuiltinSystemException` and `KeyNotFoundException`.

## Future Improvements

### Mutable Collections: Process Dict vs ETS

Currently, `Dictionary`, `HashSet`, `ResizeArray`, and `Array` use the process dictionary
pattern: `make_ref()` + `put(Ref, EntireCollection)` / `get(Ref)`. Every mutation replaces
the entire map or list — adding one key to a 10,000-entry Dictionary copies all 10,000
entries (O(N) per mutation).

**ETS (Erlang Term Storage)** is an alternative that provides O(1) in-place mutable tables.
However, the current process dict approach is arguably the **better design** for BEAM:

- **Process isolation preserved**: Mutations stay within a single process, matching the
  BEAM philosophy of isolated processes with no shared mutable state
- **No accidental sharing**: ETS tables are cross-process by default, which would break
  the isolation model that makes BEAM reliable
- **Simple cleanup**: `erase(Ref)` at scope exit vs explicit `ets:delete(Tab)` with no
  finalizers to ensure cleanup
- **Small collections are fine**: Most F# code uses small-to-medium collections where
  the full-copy overhead is negligible

| Aspect | Process Dict (current) | ETS |
| --- | --- | --- |
| Read | O(1) `get(Ref)` + O(1) `maps:get` | O(1) `ets:lookup` |
| Write | O(N) full map copy via `put` | O(1) `ets:insert` |
| Process isolation | Yes (process-local) | No (shared by default) |
| Small collections | Fast (no table overhead) | Slower (table creation cost) |
| Large collections | Slow (full copy per mutation) | Fast (in-place mutation) |
| Cleanup | `erase(Ref)` | `ets:delete(Tab)` (must be explicit) |

**Conclusion**: The process dict approach is the right default. ETS could be offered as
an opt-in optimization (e.g., via an attribute) for specific cases where large collection
mutation performance is critical, but it should not be the default since it introduces
shared mutable state — exactly what BEAM is designed to avoid.

## Open Questions

- ~~**Records vs Maps**: Erlang records are compile-time tuples (fast, but rigid).
  Maps are dynamic (flexible, slower). For F# records, maps seem more natural.~~
  **Decided**: Erlang maps. Field names as snake_case atoms, `maps:get/2` for access,
  `maps:put/3` for update. Structural equality via native `=:=`.
- **OTP project structure**: Generate a full OTP application structure with
  `rebar3`? Or just standalone `.erl` files initially?
- ~~**Interop**: How should F# code call existing Erlang/Elixir libraries?
  Fable.Core attributes like `[<Import("lists", "map")>]`?~~
  **Decided**: Three interop mechanisms: (1) `[<Import("func", "module")>]` for individual
  function imports → `module:func(Args)`, (2) `[<Emit("erlang:expr($0)")>]` for inline
  Erlang expressions, (3) `[<ImportAll("module")>]` + `[<Erase>]` interface for typed
  module bindings → `module:method(Args)`. The ImportAll pattern mirrors JS/Python but
  emits Erlang remote calls instead of attribute access.
- ~~**Testing**: Use EUnit, Common Test, or just assert in generated code?~~
  **Decided**: xUnit with `[<Fact>]` on .NET side, `Fable.Core.Testing.Assert`
  when compiled to BEAM. Same pattern as Python/Rust targets.
- **Integer wrapping granularity**: Wrap every arithmetic operation, or only at
  let-binding boundaries? Former is safer, latter is faster.
- ~~**Function name encoding**: Erlang atom names from double-backtick F# names
  currently produce URL-encoded names (e.g. `test$0020_add`). Need to decide
  on a cleaner encoding or stick with snake_case conversion.~~
  **Decided**: `sanitizeErlangName` decodes `$XXXX` hex sequences, strips apostrophes,
  converts to snake_case, collapses underscores. See "Decisions Made" above.

## Prior Art

- **Caramel** — OCaml → Erlang compiler (abandoned, but useful reference for
  ML-to-BEAM type mappings): <https://github.com/AbstractMachinesLab/caramel>
- **Gleam** — Typed functional language on BEAM, compiles to Erlang. Study its
  compiler for BEAM code generation patterns: <https://github.com/gleam-lang/gleam>
- **LFE** (Lisp Flavored Erlang) — another language targeting BEAM, shows OTP
  integration from non-Erlang language: <https://github.com/lfe/lfe>
- **Fable.Python** — the direct template for this work. Same architecture, similar
  challenges (dynamic target language, no classes for DUs, module-per-file)
