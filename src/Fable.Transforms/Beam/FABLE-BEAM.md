# Fable.Beam — F# to Erlang

Fable.Beam is Fable's Erlang/BEAM target. It compiles F# to readable `.erl`
source and supplies an Erlang runtime for F# and .NET library semantics.

| Item | Value |
| --- | --- |
| CLI | `dotnet fable --lang beam` (`--lang erlang` is an alias) |
| Output | Erlang source plus a rebar3 application scaffold |
| Minimum runtime | Erlang/OTP 25 |
| Target status | Alpha |
| Full validation | `./build.sh test beam` |

OTP 25 is the support floor. The generated code also relies on maps and named
funs, `monotonic_time`/`system_time`, `atomics`, and `uri_string`; all are
available by that release.

The compiler target is responsible for correct Erlang output. OTP bindings and
process-based application models are separate:

- [Fable.Beam](https://github.com/fable-compiler/Fable.Beam) provides typed OTP
  bindings.
- [Fable.Actor](https://github.com/fable-hub/Fable.Actor) provides an actor model.
- `MailboxProcessor`, `Async`, and `Task` provide F#-compatible APIs; they are not
  replacements for OTP behaviours. `Task` deliberately shares the cold `Async`
  runtime, so evaluating a task does not start it as a .NET hot task would.

A known private consumer is an application of about 92,000 lines of F# that uses
file and network I/O, OTP supervision trees, actors, long-running services, and
third-party packages. This is evidence that the target can support substantial
applications; the alpha status reflects the semantic gaps documented below.

## Compiler pipeline

```text
F# source
  -> FSharp2Fable
  -> Fable AST
  -> shared Fable transforms
  -> Fable2Beam
  -> Erlang AST
  -> ErlangPrinter
  -> .erl source
  -> rebar3 / Erlang compiler
```

| Path | Responsibility |
| --- | --- |
| `Beam.AST.fs` | Minimal Erlang AST |
| `Fable2Beam.fs` | Fable AST to Erlang AST |
| `Fable2Beam.Util.fs` | Shared lowering helpers |
| `Fable2Beam.Reflection.fs` | Compile-time reflection metadata |
| `Replacements.fs` | .NET and FSharp.Core calls to Erlang/runtime calls |
| `ErlangPrinter.fs` | Erlang source generation |
| `Prelude.fs` | Names, keywords, module identities, and OTP collision checks |
| `src/fable-library-beam/` | Erlang and F# runtime library |
| `src/Fable.Build/Test/Beam.fs` | .NET, generated Erlang, and entry-point validation |
| `tests/Beam/` | Target test suite and native Erlang fixtures |

The Erlang AST contains only the forms needed by the target: literals, patterns,
tuples, lists, maps, calls, functions, `case`, matches, blocks, operators,
`try`/`catch`, `receive`, and `Emit`. Target-specific syntax that does not justify
a dedicated node uses `Emit`.

## Design rules

- Prefer Erlang/OTP primitives when they preserve F# semantics.
- Handle target-specific calls in `Beam/Replacements.fs` before the JavaScript
  replacement fallback can add JavaScript-only helpers or arguments.
- Keep Fable AST to Erlang AST lowering in the transform files; put reusable
  behavior in utilities or the runtime.
- Treat the F#/.NET tests as the semantic contract. Do not weaken a passing .NET
  test to accommodate the target.
- Keep generated Erlang readable and compatible with ordinary Erlang tooling.
- Keep OTP behaviours out of the compiler. They belong in typed bindings and
  libraries built on the generated modules.

Common native mappings:

| F# operation | Erlang implementation |
| --- | --- |
| Integer arithmetic | Native arbitrary-precision arithmetic plus width wrapping |
| Bitwise operations | `band`, `bor`, `bxor`, `bsl`, `bsr`, `bnot` |
| Structural equality | `=:=` |
| Lists | Native linked lists and `lists` |
| Records and maps | Erlang maps and `maps` |
| Sets | `ordsets` plus `fable_set` |
| Hashing | `erlang:phash2/1` |
| Pattern matching | Erlang `case` clauses and patterns |

## Modules and generated projects

Erlang has one flat, global module namespace. A generated module name therefore
contains its OTP application name and source path.

| F# source | Erlang module |
| --- | --- |
| `Program.fs` in `MyApp` | `my_app_program` |
| `Misc/Util2.fs` in `MyApp` | `my_app_misc_util2` |
| `DSL.fs` in `Scriptorium.Quill` | `scriptorium_quill_dsl` |
| `fable_modules/Hedgehog.0.11/Gen.fs` | `hedgehog_gen` |

`Fable.Beam.Naming.erlangModuleName` is the single naming implementation used by
code generation and output-path generation. The CLI rejects:

- two sources that resolve to the same module atom;
- generated names that collide with modules from `erts`, `kernel`, or `stdlib`.

Exceptions:

- `fable-library-beam` keeps its established module names such as `fable_list` and
  `seq`;
- native Erlang modules referenced through interop keep their native names.

Function, field, case, and module names are sanitized to Erlang atoms. Reserved
words receive a trailing underscore. Encoded punctuation in F# compiled names uses
`_xNNNN_` segments so distinct backticked identifiers do not collapse to one atom;
remaining module-level name/arity collisions fail compilation. Reflection metadata
stores both the original F# name and the emitted atom when both are needed.

The generated project contains:

```text
<outDir>/
  rebar.config
  src/
    <app>.app.src
    main.erl
    <app>_<source>.erl
  fable_modules/
    fable-library-beam/
      rebar.config
      src/
        fable_library_beam.app.src
        ...
```

Fable regenerates its own scaffold files. An existing user-owned `rebar.config`
is left unchanged.

### Entry point

The last F# source file owns module-level actions and the program entry point. The
CLI emits `src/main.erl` as a stable shim:

```sh
erl -noshell -pa _build/default/lib/*/ebin \
  -eval 'main:main([])' -s init stop
```

`main:main/0` forwards to the generated entry module. `main:main/1` forwards command
line arguments. An integer F# entry-point result becomes the VM exit code. The shim
also sets `standard_io` and `standard_error` to Unicode.

## Runtime representation

| F# value | Erlang representation | Notes |
| --- | --- | --- |
| `unit` | `ok` | Non-final unit expressions are removed by the printer |
| `bool` | `true` / `false` | Atoms |
| Signed and unsigned integers | `integer()` | Width restored after operations that can overflow |
| `bigint` | `integer()` | No width wrapping |
| `float` | `float()` | Erlang floating point |
| `decimal` | scaled `integer()` | Value multiplied by 10^28; `GetBits` emits the smallest equivalent scale |
| `string` | UTF-8 `binary()` | Not a charlist |
| `char` | `integer()` | Unicode codepoint; see known limitations |
| Enum | `integer()` | Casts erase to the underlying number |
| `[<StringEnum>]` | atom | `[<CompiledValue>]` literals remain literals |
| Tuple | tuple | `{A, B}` |
| `list<'T>` | list | Native linked list |
| `array<'T>` | process-dictionary reference to a list | Mutable and process-local |
| `byte[]` | `{byte_array, Size, AtomicsRef}` | O(1) reads and writes through `atomics` |
| `seq<'T>` | Fable sequence runtime | Compiled `Seq.fs`/`Seq2.fs` preserve lazy paths |
| `Map<'K,'V>` | map | Native key ordering applies |
| `Set<'T>` | ordset | Sorted native list |
| `option<'T>` | erased value or `undefined` | Ambiguous nested/generic cases use `{some, Value}` |
| `Result<'T,'E>` | `{ok, Value}` / `{error, Error}` | Erlang convention |
| Nullary union case | atom | Sanitized case tag |
| Union case with fields | tuple | `{case_tag, Field1, ...}` |
| Record / anonymous record | map | Sanitized field atoms |
| Immutable class | map | Used when construction does not require self-reference or mutation |
| Mutable/self-referencing class | process-dictionary reference | State is process-local |
| Interface / object expression | map of values and closures | Calls dispatch through map entries |
| Exception | map | Includes `message` and, for custom exceptions, `exn_type` |
| Function | Erlang fun | Curry/uncurry adapters preserve identity where supported |
| Local mutable / ref cell | process-dictionary reference | Erased at scope exit where possible |

## Implemented features

The checklists record current capabilities, not development phases.

### Language and code generation

- [x] Literals, tuples, lists, maps, functions, delegates, and partial application
- [x] Arithmetic, logical, bitwise, relational, and equality operators
- [x] `if`, loops, sequence expressions, ranges, and pipelines
- [x] Pattern matching, active patterns, decision trees, and guards
- [x] Recursive and mutually recursive functions
- [x] Tail calls and named recursive Erlang funs
- [x] Records, anonymous records, discriminated unions, options, results, and enums
- [x] Class construction and common instance/member calls; interfaces and object expressions
- [x] Custom exceptions, `AggregateException`, `try`/`with`, `try`/`finally`, `raise`, `failwith`, and `failwithf`
- [x] Modules, nested modules, imports, exports, and cross-file calls
- [x] Module-qualified names and OTP module collision detection
- [x] Quotations and derived quotation patterns
- [x] Compile-time and runtime reflection metadata
- [x] `FABLE_COMPILER_BEAM` conditional compilation symbol

### Numerics, text, and time

- [x] Fixed-width signed and unsigned integer wrapping
- [x] `int64`, `uint64`, `bigint`, and decimal arithmetic
- [x] Integer and floating-point conversions and parsing
- [x] String, character, encoding, regular-expression, and URI operations
- [x] `sprintf`, `printf`, `printfn`, `eprintfn`, `failwithf`, and `String.Format`
- [x] Static dispatch to custom `ToString()` for direct calls, `string`, and `%O`
- [x] `DateTime`, `DateTimeOffset`, `DateOnly`, `TimeOnly`, and `TimeSpan`
- [x] `Guid`, `Random`, `Stopwatch`, `BitConverter`, and environment APIs

### Collections

- [x] `List`, `Seq`, `Array`, and `Array.Parallel`
- [x] `Map` and `Set`
- [x] `ResizeArray`, `Dictionary`, `HashSet`, `Queue`, and `Stack`
- [x] Mutable byte arrays backed by `atomics`
- [x] Structural collection equality and comparison on supported element shapes
- [x] Enumeration through lists, references, maps, and mutable collections

### Effects and concurrency

- [x] `Async` computation expressions
- [x] `Task` computation expressions mapped to the Async runtime
- [x] `Async.StartChild`, `AwaitTask`, `Parallel`, `Sequential`, and continuations
- [x] Cancellation tokens and cancellation-aware sleep
- [x] `MailboxProcessor`
- [x] Observables and events
- [x] Parallel array operations using spawned worker processes

### Interop and tooling

- [x] `[<Emit>]` inline Erlang expressions
- [x] `[<Import>]` native module calls
- [x] `[<ImportAll>]` with erased interfaces for typed module bindings
- [x] Erlang keyword and quoted-atom escaping
- [x] rebar3 scaffold generation
- [x] Runtime-library placement under `fable_modules`
- [x] Quicktest support
- [x] .NET, generated Erlang, native Erlang, and entry-point tests

## Core semantics

### Replacements

Call lowering follows this order:

```text
Beam.Replacements.tryCall
  -> Beam-native operator or OTP/runtime call
  -> JavaScript replacement fallback when target-neutral
```

BEAM replacements own numerics, equality and comparison, collections, conversion,
formatting, Async/Task, reflection, mutable storage, and Erlang interop. This avoids
JavaScript-only modules and injected comparers/adders.

### Lowering invariants

| Area | Invariant |
| --- | --- |
| Curried calls | Apply one argument at a time. Combining curried arguments into one Erlang call changes arity. |
| Unit arguments | Remove trailing unit parameters and their call-site arguments symmetrically. |
| Expression blocks | Hoist leading matches before calls, operators, and literals; Erlang argument positions cannot contain Fable statement blocks. |
| Recursion | Emit self-recursive lambdas as named funs; emit a mutual-recursion group through one tagged dispatcher. |
| BIF names | Qualify known BIF calls with `erlang:` and emit `no_auto_import` when a generated local function shares an auto-imported name. |
| Arrays at FFI boundaries | Cancel an inline `new_ref`/`get` pair only when every use of that literal argument is dereferenced. Bound arrays retain reference semantics. |
| Mutable locals | Erase generated process-dictionary keys at scope exit where their lifetime is known. |
| Module initialization | Preserve declaration order and isolate each initializer's Erlang variables before merging it into `main/0`. |
| Unit expressions | Remove non-final bare `ok` expressions without removing a final function result. |
| Classes | Choose a self-contained map only when fields and stored closures do not require mutation or `this`; otherwise use a process-local reference. |

### Fixed-width integers

Erlang integers are arbitrary precision. `fable_int.erl` restores .NET widths with
bit-syntax wrapping:

```erlang
wrap_i32(N) ->
    <<V:32/signed-integer>> = <<N:32/signed-integer>>,
    V.
```

The compiler wraps operations that can leave the type's range: addition,
subtraction, multiplication, left shift, negation, complement, and narrowing
conversion. Shift counts are masked to the .NET width. Operations that cannot grow
an in-range value remain native. `bigint` and decimal are not width-wrapped.

Unsigned values keep their unsigned range, so `UInt64.MaxValue` is represented as
`18446744073709551615` and right shift is logical.

### Equality, ordering, and hashing

- Native `=:=` supplies deep equality for numbers, atoms, binaries, tuples, lists,
  and maps.
- `fable_comparison:hash/1` supplies structural hashing through `erlang:phash2/1`;
  array `GetHashCode()` hashes the storage handle to preserve identity across mutation.
- Direct comparison of a statically known union uses
  `fable_comparison:compare_union/3` with case declaration order.
- Direct union relational operators, `compare`, collection `min`/`max`, and
  `List`/`Array` sorting operations use the same declaration-order comparison
  when the union type is statically known.
- Function physical equality unwraps compiler-generated curry/eta adapters before
  comparing the underlying fun.

The runtime union representation does not carry the declaration index. Generic
comparison paths that lack static union type information therefore fall back to
Erlang term order. See the roadmap.

### Mutability and process locality

Local mutables, non-byte arrays, mutable collections, and mutable class state use
`make_ref()` keys in the process dictionary. Benefits and constraints for those
process-dictionary-backed values:

- mutation is isolated to the owning process;
- a value sent to another process does not carry its mutable state;
- non-byte list/map collection updates replace the stored collection;
- mutable local keys are erased when their scope exits where code generation can
  prove the lifetime.

Module-level mutables use module-qualified process-dictionary keys and initialize
through the generated module's `main/0`. Immutable module values that read a mutable
are snapshotted in declaration order. This is correct only in a process that has run
that initializer.

Byte arrays are the exception. A `byte[]` carries an `atomics` reference in its
`{byte_array, Size, AtomicsRef}` value. Sending that value to another process shares
the backing storage, and `byte_array_set/3` mutations are visible in both processes.

Use OTP processes and message passing for shared application state. Do not pass a
process-dictionary-backed mutable F# object to another process and expect
shared-object semantics; treat a byte array passed between processes as explicitly
shared mutable state.

### Exceptions

F# and built-in .NET exceptions use maps with a nominal `exn_type` atom. Built-in
type tests follow the .NET exception hierarchy, so typed handlers distinguish sibling
exceptions while accepting derived exceptions through their base type.

Generated Erlang catches the native `Class:Reason:Stacktrace` triple. `reraise` and
unmatched filtered handlers use `erlang:raise/3`, preserving the original Erlang
exception class, reason, and stacktrace.

### Async, Task, and MailboxProcessor

`Async<'T>` is a cold continuation-passing function:

```text
Async<T> = fun(Context) -> ok end
Context  = #{on_success, on_error, on_cancel, cancel_token}
```

`RunSynchronously`, `StartImmediate`, `StartWithContinuations`, and sequential
composition execute in the caller's process so process-local mutable state remains
visible. `Async.Parallel`, `Async.StartChild`, and parallel array operations spawn
workers; those workers do not share the caller's process dictionary.

Cancellation tokens use shared `atomics` state and can cross process boundaries.
Registration, cancellation, and `CancelAfter` are serialized by one runtime broker.
`Cancel()` invokes callbacks in the cancelling process; `CancelAfter` uses a
short-lived worker. A cross-process callback must therefore use process-safe state
such as PIDs and messages rather than captured process-dictionary-backed mutables.
Dispose a registration when its source may never cancel, because the broker retains
active callbacks until cancellation or disposal.

`Async.StartChild` and `Async.Parallel` do not implicitly propagate their parent
cancellation token. Pass or capture a token explicitly when a spawned computation
must observe it.

`Task` uses the same runtime and is not a separate hot-task abstraction.
`MailboxProcessor` also uses in-process CPS and a process-local queue. Real OTP
processes, `gen_server`, supervisors, ETS, and distribution come from external
bindings.

### Formatting and Unicode

Strings are UTF-8 binaries. Generated console calls use Unicode-aware Erlang `io`
formats. The generated `main.erl` configures both standard devices; native launchers
must do the same:

```erlang
ok = io:setopts(standard_io, [{encoding, unicode}]),
ok = io:setopts(standard_error, [{encoding, unicode}]).
```

`%O` receives statically selected converters for values with a custom
`System.Object.ToString()` override. `%A` uses runtime term shape because records,
unions, primitives, and mutable references do not carry complete type metadata.

### Reflection

Generated reflection functions return maps containing full names, generic
arguments, record fields, or union cases. Recursive record/union metadata is lazy.
Field and case entries retain both source names and emitted Erlang atoms.

`fable_reflection.erl` implements record, union, tuple, function, and type queries,
plus construction and field/case access. Erased runtime values still limit dynamic
type tests and formatting where two F# types have the same Erlang shape.

Runtime `:?` tests for ordinary discriminated unions match the compiled case tags
and exact tuple arities; fieldless cases match their bare atoms. These tests accept
arbitrary terms safely and do not inspect payload fields. Generic arguments and
nominal union identity are erased, so types with identical emitted case tags and
arities cannot be distinguished by these tests.

### Interop

| F# | Erlang |
| --- | --- |
| `[<Import("map", "lists")>]` | `lists:map(...)` |
| `[<Emit("erlang:self()")>]` | Inline Erlang expression |
| `[<ImportAll("gen_server")>]` erased interface | Typed remote calls such as `gen_server:call(...)` |
| `[<StringEnum>]` case | Erlang atom |

Interop values use their actual Erlang representation. Public APIs that expose
records, unions, options, arrays, or mutable objects therefore depend on the target
ABI described above.

## Runtime coverage

| Area | Runtime modules |
| --- | --- |
| Core values | `fable_option`, `fable_result`, `fable_convert`, `fable_comparison`, `fable_utils` |
| Collections | `fable_list`, `fable_seq`, `fable_map`, `fable_set`, `fable_resize_array`, `fable_dictionary`, `fable_hashset`, `fable_queue`, `fable_stack` |
| Numerics | `fable_int`, `fable_decimal`, `fable_bit_converter`, `fable_random` |
| Text | `fable_string`, `fable_char`, `fable_regex` |
| Date and time | `fable_date`, `fable_date_offset`, `fable_date_only`, `fable_time_only`, `fable_timespan`, `fable_stopwatch` |
| Effects | `fable_async_builder`, `fable_async`, `fable_cancellation`, `fable_mailbox`, `fable_parallel` |
| Events | `fable_observable`, `fable_event` |
| Metadata | `fable_reflection`, `fable_quotation` |
| Platform values | `fable_guid`, `fable_uri`, `fable_environment` |

## Validation

Fast iteration:

```sh
./build.sh quicktest beam
```

Build the runtime library:

```sh
./build.sh fable-library --beam
```

Run the complete target validation:

```sh
./build.sh test beam
```

The full command:

1. runs the F# tests on .NET;
2. builds `fable-library-beam`;
3. transpiles `tests/Beam/` to `temp/tests/Beam/`;
4. compiles the generated project with rebar3;
5. runs every exported `test_*` function through `erl_test_runner`;
6. compiles and runs entry-point fixtures through the generated `main.erl` shim.

The passing-test count is intentionally omitted because it changes whenever the
suite grows.

## Known limitations

| Area | Current behavior |
| --- | --- |
| Union ordering | Generic comparison, nested union fields, and union keys/elements in `Map`/`Set` can use Erlang atom order instead of declaration order. |
| Options | Erasure can still conflate `None`, `Some null`/`Some undefined`, and some nested option paths after static type information is lost. |
| `char` | A generic or `obj`-erased character is an integer at runtime, so `string` and `%A` can print its codepoint. UTF-16 surrogate behavior is not complete. |
| Structured formatting | `%A` reconstructs values from term shape. Record field order, original names, erased options, sets, chars, refs/arrays, decimals, and date/time values can differ from .NET output. |
| Identifiers | Instance members split across declaration paths can still sanitize to the same Erlang function name and arity. |
| Type tests | Erlang cannot distinguish integer widths or unrelated F# types with the same runtime shape. Some interface/class downcasts and abstract/base dispatch paths are unsupported. |
| Classes and structs | Mutable record fields, class reference equality, some self-referencing/base constructors, mutually recursive class hierarchies, and default struct construction remain incomplete. |
| Module initialization | Module-level mutable values and snapshots exist only in a process that ran the generated module `main/0`; ordinary library calls and other processes can read `undefined`. |
| Mutable collections | Non-byte arrays and mutable collections are process-local. List/map-backed mutation can be O(N). |
| Collection comparers | `Dictionary` and `HashSet` constructors ignore custom `IEqualityComparer` instances and use native structural keys. |
| Function identity | Curry/eta identity support covers compiler-generated adapters of arity 2 through 7 and statically known function types; generic call sites can fall back to native fun identity. |
| Numeric APIs | Special floating-point values need parity work. |
| Formatting APIs | `FormattableString`, some custom `TimeSpan` formats, and width-sensitive negative hexadecimal formatting are incomplete. |
| Defaults and null | `Unchecked.defaultof` and null semantics differ for strings, structs, and erased values. |
| Recursive values | Recursive value bindings that lower through `Lazy` and some inline module-value side effects are incomplete. |
| Cancellation | Cross-process callbacks cannot safely mutate captured process-local values. Callback exceptions are suppressed, and `CancellationTokenSource.Dispose()` remains a no-op. |

For `char` conversion in generic code, making the function `inline` or using a
concrete `char` annotation keeps the type available at the call site. Other entries
above have no general source-level workaround and should remain visible in tests.

## Related projects

- [Fable.Beam](https://github.com/fable-compiler/Fable.Beam) — typed Erlang/OTP bindings
- [Fable.Actor](https://github.com/fable-hub/Fable.Actor) — actor model on the bindings
- [Gleam](https://github.com/gleam-lang/gleam) — typed language compiling to Erlang
- [Caramel](https://github.com/AbstractMachinesLab/caramel) — OCaml to Erlang compiler
- [LFE](https://github.com/lfe/lfe) — Lisp on the BEAM

## Roadmap

The CLI reports the target as alpha. The next steps are ordered by semantic risk,
not by the age of the feature. Beta means that the documented supported surface is
reliable enough for broader use; it does not require complete .NET parity. This is
the same maturity model used by the established targets: target-specific behavior,
unsupported APIs, and disabled parity cases can remain when they are deliberate and
visible rather than silent correctness failures.

### Correctness priorities for beta

| Priority | Gap | Suggested direction |
| --- | --- | --- |
| P0 | Union declaration ordering is incomplete | Thread union-aware comparers through nested comparison and ordered collections. If type-directed routing cannot cover generic containers, define a versioned DU/collection representation change. |
| P0 | Option erasure loses states in generic and null-like paths | Carry the nested-option decision through replacements and collection helpers, or adopt an unambiguous tagged form where erasure is unsafe. |
| P0 | Module initialization is process-dependent | Define library initialization semantics. Prefer explicit generated initialization invoked by entry points/process owners; use global storage only if cross-process mutation is intentionally supported. |
| P0 | Object-model gaps affect valid F# | Fix silent wrong-code paths in the claimed object-model surface. Keep unsupported class and struct forms as explicit exclusions until implemented. |
| P0 | Numeric APIs have correctness gaps | Fix incorrect results in claimed numeric APIs. Missing APIs can remain documented exclusions; restore regression tests as implementations land. |

### Fidelity and diagnostics

| Priority | Gap | Suggested direction |
| --- | --- | --- |
| P1 | Disabled or commented parity cases are not an auditable support boundary | Remove stale skips whose underlying defect is fixed. Track relevant remaining cases as tests, documented exclusions, or linked issues without requiring complete parity for beta. |
| P1 | `%O`, `%A`, interpolation, and `String.Format` have separate type-information needs | Build one compiler-generated argument-slot plan that records value, width, printer, and thunk arguments plus optional static formatters. Reuse it across all formatting entry points. |
| P1 | Runtime shapes cannot distinguish several F# types | Pass compact type descriptors or generated recursive formatters at typed call sites. Treat self-describing record/union values as a versioned ABI option, not an incidental formatting patch. |
| P1 | Type tests and downcasts are shape-based | Add compact type tokens only where F# semantics require nominal identity; keep ordinary data representations untagged where possible. |
| P1 | Warnings do not define the supported surface | Document intentional deviations, make unsupported features actionable diagnostics, and test diagnostic text and source ranges. |
| P1 | OTP/version support is a single minimum statement | Run CI against OTP 25 and the current supported OTP release; publish the tested range. |

### Performance and operability

| Priority | Gap | Suggested direction |
| --- | --- | --- |
| P2 | Mutable list/map storage copies on update | Benchmark realistic collection sizes. Keep process isolation as the default; add an explicit ETS-backed type or optimization only for measured hot paths. |
| P2 | Curry/eta identity adds adapter work | Benchmark generated functions and extend marked adapters beyond arity 7 only when required by real code. |
| P2 | Generated Erlang has no published size/runtime baseline | Track compile time, BEAM file size, startup, allocation, collection workloads, Async, and message-heavy applications. |
| P2 | Application-scale evidence is private and not reproducible in this repository | Keep the downstream application as a compatibility signal and add a public packaged-compiler smoke test using Fable.Beam/Fable.Actor. |

### Ecosystem work

- Keep OTP bindings in Fable.Beam rather than adding OTP-specific abstractions to the
  compiler.
- Maintain a compatibility matrix for Fable, Fable.Beam, Fable.Actor, Erlang/OTP,
  and rebar3.
- Add end-to-end examples for a command-line program, a rebar3 library, an OTP
  application, supervised processes, and distribution.
- Test generated modules as dependencies of hand-written Erlang applications, not
  only as application entry points.
- Document the public Erlang ABI for unions, records, options, exceptions,
  functions, and mutable values before external packages depend on it.

### Suggested beta criteria

- There is no known silent miscompilation, data corruption, or process-safety defect
  in the documented supported surface.
- Remaining semantic and API gaps are deliberate, documented exclusions and have
  regression tests where practical; complete .NET parity is not required.
- Supported constructs do not silently compile to an `unsupported_*` placeholder.
  Statically detectable unsupported constructs produce an actionable diagnostic.
- The complete BEAM suite passes on the oldest and newest supported OTP releases.
- Packaged `dotnet fable --lang beam` output builds and runs in a clean consumer
  project without repository-local files.
- Module initialization and mutation have documented, tested behavior for the
  supported executable, library, and spawned-process scenarios.
- The generated-value ABI used by supported interop scenarios is documented, and
  representation changes are called out.
- At least one non-trivial downstream OTP application remains green, with a public
  packaged-consumer smoke test covering the reproducible integration path.

### Suggested stable-target criteria

- No known high-severity semantic divergence remains in supported F# language
  features or core runtime APIs.
- F#/.NET parity exclusions are reviewed, documented, and small enough to form a
  deliberate support policy.
- The Erlang ABI has a compatibility and versioning policy.
- OTP, rebar3, Fable.Beam, and Fable.Actor compatibility ranges are published and
  exercised in CI.
- Diagnostics fail early for unsupported code and point to a documented alternative.
- Performance baselines show no blocking regressions in representative compute,
  collection, Async, and OTP workloads.
- Release, upgrade, and deprecation procedures have been exercised across multiple
  Fable releases and real consumer applications.
