# Fable.Beam — F# on the BEAM

Draft plan for adding an Erlang/BEAM target to Fable.

## Motivation

- Learn the BEAM/OTP platform deeply by building a compiler target (the same
  approach that made Fable.Python a success for learning Python)
- Bring F#'s type system, pattern matching, and computation expressions to the
  BEAM ecosystem
- Map F#'s agent model (`MailboxProcessor`) to OTP's `gen_server` — a natural fit
- Explore what F# + OTP supervision trees + hot code reloading could look like

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
    ↓  Fable2Beam (NEW)
Erlang AST
    ↓  ErlangPrinter (NEW)
.erl source files
```

## New Files

### Compiler transforms (`src/Fable.Transforms/Beam/`)

|        File        |                    Purpose                    |               Reference               | Status |
| ------------------ | --------------------------------------------- | ------------------------------------- | ------ |
| `Beam.AST.fs`      | Erlang AST type definitions (minimal for now) | `Python/Python.AST.fs` (~1,500 lines) | Done   |
| `Fable2Beam.fs`    | Combined transforms + compiler (single file)  | `Fable2Php.fs` pattern                | Done   |
| `Replacements.fs`  | .NET BCL → Erlang library mappings (stubs)    | `Python/Replacements.fs`              | Done   |
| `ErlangPrinter.fs` | Erlang AST → `.erl` source code               | `Python/PythonPrinter.fs`             | Done   |

Phase 1 uses a single `Fable2Beam.fs` (following the PHP pattern). Can split into
`Fable2Beam.Types.fs`, `Fable2Beam.Util.fs`, `Fable2Beam.Transforms.fs`, and
`BeamCompiler.fs` later as complexity grows (following the Python pattern).

### Runtime library (`src/fable-library-beam/`)

Erlang modules implementing F# core types:

| Module | Purpose | Notes | Status |
| --- | --- | --- | --- |
| fable_option.erl | Option | Some(x) = x, None = undefined | Done |
| fable_list.erl | FSharpList | fold, find, choose, collect, etc. | Done |
| fable_map.erl | FSharpMap | Erlang native maps | Done |
| fable_seq.erl | Seq / IEnumerable | Eager lists, delay/singleton/unfold | Done |
| fable_string.erl | String utilities | Erlang binaries | Done |
| fable_result.erl | Result | {ok, V} or {error, E} | Planned |
| fable_choice.erl | Choice | Tagged tuples | Planned |
| fable_set.erl | FSharpSet | sets or gb_sets | Planned |
| fable_decimal.erl | decimal | Needs a library or custom impl | Planned |
| fable_guid.erl | Guid | UUID generation | Planned |
| fable_date.erl | DateTime | calendar module | Planned |
| fable_async.erl | Async | Processes + message passing | Planned |
| fable_mailbox.erl | MailboxProcessor | gen_server wrapper | Planned |
| fable_reflection.erl | Reflection helpers | Type info at runtime | Planned |
| fable_util.erl | General utilities | Comparison, equality, hashing | Planned |

### Registration & CLI (modified existing files) -- All Done

|                      File                      |                     Change                      | Status |
| ---------------------------------------------- | ----------------------------------------------- | ------ |
| `src/Fable.AST/Plugins.fs`                     | Added `Beam` to `Language` DU                   | Done   |
| `src/Fable.Compiler/Util.fs`                   | Added `.erl` file extension                     | Done   |
| `src/Fable.Compiler/ProjectCracker.fs`         | Added Beam library path                         | Done   |
| `src/Fable.Cli/Entry.fs`                       | Added `--lang beam` argument parsing            | Done   |
| `src/Fable.Cli/Pipeline.fs`                    | Added `Beam.compileFile` dispatch               | Done   |
| `src/Fable.Transforms/Replacements.Api.fs`     | Added Beam to `createMutablePublicValue`        | Done   |
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
| `option<T>`      | `{some, V} \           | none`                                      | Or use `undefined` atom      |
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
| **Interfaces**                    | Behaviour callbacks or dispatch maps              | Protocol pattern                     |
| **Mutability** (`ref`, `mutable`) | Process dictionary, ETS, or process state         | Agent pattern                        |
| **Exceptions**                    | `throw`/`catch` with tagged tuples                | Error tuples (Erlang way)            |
| **Generics**                      | Erased (Erlang is dynamically typed)              | —                                    |
| **Currying**                      | Lambda wrapping (same as Python target)           | —                                    |
| **Nested modules**                | Flat module names: `My_Module_Sub`                | One file per module                  |
| **Computation expressions**       | Transformed at Fable AST level (before target)    | —                                    |

## The Interesting Parts: OTP Integration

### MailboxProcessor → gen_server

This is the marquee feature. F#:

```fsharp
let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop state =
        async {
            let! msg = inbox.Receive()
            match msg with
            | Increment n -> return! loop (state + n)
            | Get channel -> channel.Reply(state); return! loop state
        }
    loop 0)
```

Could generate an Erlang `gen_server`:

```erlang
-module(counter).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_cast/2, handle_call/3]).

start_link() -> gen_server:start_link(?MODULE, 0, []).
init(State) -> {ok, State}.

handle_cast({increment, N}, State) -> {noreply, State + N};
handle_call(get, From, State) -> {reply, State, From}.
```

### Async → Processes

|            F#            |             Erlang              |
| ------------------------ | ------------------------------- |
| `Async.Start`            | `spawn_link(fun() -> ... end)`  |
| `Async.RunSynchronously` | Direct execution or `receive`   |
| `Async.Parallel`         | Spawn multiple, collect results |
| `Async.Sleep`            | `timer:sleep(Ms)`               |
| `let! x = ...`           | Message passing with `receive`  |

### Supervision (future — needs F# API design)

Possible directions:

```fsharp
// Option A: Attributes
[<Supervisor(Strategy.OneForOne)>]
module MyApp =
    [<Child>]
    let counter = CounterAgent.start

// Option B: Computation expression
let app = supervisor {
    strategy OneForOne
    child "counter" CounterAgent.start
    child "logger" LoggerAgent.start
}

// Option C: Direct OTP interop via Fable.Core attributes
[<Import("gen_server", "start_link")>]
let startLink: ... = nativeOnly
```

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
- [x] NewUnion → tagged tuples `{Tag, Field1, Field2, ...}`
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

1. Runs all tests on .NET via `dotnet test` (273 tests)
2. Compiles tests to `.erl` via Fable
3. Compiles `.erl` files with `erlc`
4. Runs an Erlang test runner (`erl_test_runner.erl`) that discovers and executes all `test_`-prefixed functions (273 Erlang tests pass)

| Test File | Tests | Coverage |
| --- | --- | --- |
| ArithmeticTests.fs | 28 | Arithmetic, bitwise, logical, comparison, Int64, unary negation |
| ListTests.fs | 35 | List operations, head/tail, map/filter/fold, append, sort, choose, collect, find, zip |
| UnionTypeTests.fs | 10 | All union tests including match-in-expression, Either with string interpolation |
| PatternMatchTests.fs | 10 | All pattern match tests including option with string interpolation |
| FnTests.fs | 15 | All function tests including recursive lambdas and mutual recursion |
| RecordTests.fs | 9 | Creation, update, float fields, nesting, function params, anonymous records, structural equality |
| StringTests.fs | 29 | String methods, interpolation, concat, substring, replace, split, trim, pad, etc. |
| TryCatchTests.fs | 8 | try/catch, failwith, exception messages, nested try/catch |
| OptionTests.fs | 13 | Option.map/bind/defaultValue/filter/isSome/isNone, Option module functions |
| ConversionTests.fs | 11 | int/float/string conversions, int-to-float, float-to-int, toString, parse |
| LoopTests.fs | 7 | for loops, while loops, nested loops, mutable variables |
| ArrayTests.fs | 19 | Array literal, map/filter/fold, mapi, append, sort, indexed, length |
| TupleTests.fs | 10 | Tuple creation, destructuring, fst/snd, equality, nesting |
| MapTests.fs | 21 | F# Map create/add/remove/find/containsKey/count, iteration, fold, filter |
| SeqTests.fs | 47 | Seq.map/filter/fold/head/length/append/concat/distinct/take/skip/unfold/init/scan/zip/etc. |
| SudokuTests.fs | 1 | Integration test: constraint-propagation Sudoku solver using Seq, Array, ranges |
| **Total** | **273** | |

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
- [x] `array<T>` → Erlang lists (arrays represented as lists for simplicity)
- [x] `Map<K,V>` → Erlang native `#{}` maps, `maps:` module calls + `fable_map.erl`
- [ ] `Set<T>` → Erlang `sets` / `gb_sets` (not yet implemented)
- [x] `Seq<T>` → eager Erlang lists with `fable_seq.erl` library
- [x] `fable-library-beam` runtime: `fable_list.erl`, `fable_map.erl`, `fable_string.erl`, `fable_option.erl`, `fable_seq.erl`
- [x] Range expressions: `[1..n]` → `lists:seq(1, n)`, `[1..2..n]` → `lists:seq(1, n, 2)`
- [x] Array indexing: `arr.[i]` → `lists:nth(i + 1, arr)` (0-based to 1-based)
- [x] Array comprehensions: `[| for i in 0..n -> expr |]` via Seq desugaring

**Design decisions**:

- Sequences are **eager** (evaluated immediately as lists). `Seq.delay(f)` calls `f(ok)`.
  This simplifies the implementation; lazy sequences can be added later if needed.
- Seq operations intercepted in Beam Replacements (not JS fallback) to avoid
  injected comparers/adders that Erlang doesn't need.
- Complex operations in `fable_list.erl`/`fable_seq.erl`, simple BIF mappings via `emitExpr`.
- All multi-arg callbacks use curried application `(Fn(A))(B)` to match Fable's compilation.
- Integration tested with a Sudoku solver (SudokuTests.fs) using Seq, Array, ranges, and
  array comprehensions.

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

### Phase 6: Error Handling -- PARTIALLY COMPLETE

- [x] try/with → try/catch with `erlang:error` for exceptions
- [x] `failwith` → `erlang:error(<<"message">>)`
- [x] Exception message access via `#{message => Reason}` map wrapping
- [x] Nested try/catch works
- [ ] `Result<T,E>` integration with Erlang `{ok,V}/{error,E}` convention

### Phase 7: Async & Processes

- [ ] Basic `async { }` → process spawn
- [ ] `let!` → message receive
- [ ] `Async.Start` → `spawn_link`
- [ ] `Async.Parallel` → multi-process fan-out
- [ ] Cancellation tokens → process monitors

### Phase 8: MailboxProcessor → gen_server

The crown jewel.

- [ ] `MailboxProcessor.Start` → `gen_server:start_link`
- [ ] `inbox.Receive()` → `handle_cast`/`handle_call`
- [ ] `agent.Post` → `gen_server:cast`
- [ ] `agent.PostAndAsyncReply` → `gen_server:call`
- [ ] `AsyncReplyChannel` → `gen_server` `From` reply

### Phase 9: OTP Patterns (exploratory)

- [ ] Supervision trees
- [ ] Application behaviour
- [ ] Hot code reloading
- [ ] Distribution / multi-node

### Phase 10: Ecosystem

- [ ] Build integration (`rebar3` or `mix` project generation)
- [x] Test suite (`tests/Beam/` — 273 .NET tests + 273 Erlang tests, `./build.sh test beam`)
- [x] Erlang test runner (`tests/Beam/erl_test_runner.erl` — discovers and runs all `test_`-prefixed arity-1 functions)
- [x] `erlc` compilation step in build pipeline (per-file with graceful failure)
- [x] Quicktest setup (`src/quicktest-beam/`, `Fable.Build/Quicktest/Beam.fs`)
- [ ] Documentation

## Erlang AST Sketch (Full Target)

The current implementation in `Beam.AST.fs` is a minimal subset. Below is the full
target AST to expand towards as more features are implemented:

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
  Beam overrides equality to use native `=:=`; everything else falls through to JS.
- **File structure**: Single `Fable2Beam.fs` for Phase 1 (PHP pattern), split later
  as complexity grows (Python pattern)
- **DU representation**: Tagged tuples `{Tag, Field1, ...}` with integer tags,
  accessed via `element/2`. Matches Erlang convention for variant types.
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
- **Function name sanitization**: `sanitizeErlangName` in `Fable2Beam.fs` decodes
  `$XXXX` hex sequences from F# compiled names (e.g. `$0020` → space), strips
  apostrophes, converts to snake_case, and collapses/trims underscores. Applied to
  all function names and import selectors. Example:
  `test$0020infix$0020add$0020can$0020be$0020generated` → `test_infix_add_can_be_generated`
- **Output filenames**: Snake_case, following the Python target pattern. Uses
  `Naming.applyCaseRule Core.CaseRules.SnakeCase` in `Pipeline.fs` Beam module.
  Erlang requires module name to match filename, so `ArithmeticTests.fs` →
  `arithmetic_tests.erl` with `-module(arithmetic_tests).`
- **Inline assertions**: `assertEqual`/`assertNotEqual` (and their `Testing_equal`/
  `Testing_notEqual` variants) are inlined as `case Actual =:= Expected of true -> ok;
  false -> erlang:error({assert_equal, Expected, Actual}) end` — no runtime dependency
  on a util module.
- **Unit parameters**: Erlang unused variable warnings suppressed by prefixing unit
  parameters with `_` via `toErlangVar` in `Fable2Beam.fs`.
- **Block hoisting**: When `Let` bindings produce `Block [Match(...); body]` expressions,
  these Blocks are invalid inside Erlang argument positions (Call, Apply, BinOp). The
  `extractBlock` / `hoistBlocksFromArgs` / `wrapWithHoisted` helpers extract leading
  assignments and hoist them before the enclosing expression. This fixes
  match-in-expression patterns like `match x with ... |> equal "abc"`.
- **Recursive lambdas**: Self-recursive `let rec f x = ... f (x-1) ...` inside function
  bodies generates Erlang named funs: `fun F(X) -> ... F(X-1) ... end` (OTP 17+).
  Detected via `containsIdentRef` which checks if a lambda body references its own
  binding ident. Mutual recursion (`let rec ... and ...`) not yet supported.
- **String interpolation**: `$"text: {value}"` generates
  `iolist_to_binary([<<"text: ">>, integer_to_binary(Value)])`. Integer values use
  `integer_to_binary/1`, string values pass through, other types use
  `io_lib:format("~p", [Value])`. Note: `io_lib:format` needs a charlist format string,
  not a binary — uses `binary_to_list(<<"~p">>)` to convert. String concatenation from
  Fable's Replacements (`string:concat`) is intercepted and replaced with
  `iolist_to_binary([A, B])` since `string:concat` returns charlists, not binaries.
  TODO: Move `to_string` conversion to `fable-library-beam/strings.erl`.
- **Assert temp variables**: Complex expressions in assertEqual/assertNotEqual are stored
  in temp variables (`Assert_actual_N`, `Assert_expected_N`) to avoid duplicate
  evaluation and Erlang "unsafe variable" errors from variable bindings inside case
  branches that get duplicated in error messages.

## Open Questions

- ~~**Records vs Maps**: Erlang records are compile-time tuples (fast, but rigid).
  Maps are dynamic (flexible, slower). For F# records, maps seem more natural.~~
  **Decided**: Erlang maps. Field names as snake_case atoms, `maps:get/2` for access,
  `maps:put/3` for update. Structural equality via native `=:=`.
- **OTP project structure**: Generate a full OTP application structure with
  `rebar3`? Or just standalone `.erl` files initially?
- **Interop**: How should F# code call existing Erlang/Elixir libraries?
  Fable.Core attributes like `[<Import("lists", "map")>]`?
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
