# Pending Fixes and Actions

## Create PR for #4080 (Watch mode file lock - IMPLEMENTED, needs push)

**Branch (local)**: `repo-assist/fix-issue-4080-watch-file-lock`
**Implementation**: COMPLETE (committed to local branch in workspace)
**Status**: Branch exists, needs push + PR creation via safeoutputs

### Changes made:
- `src/Fable.Compiler/File.fs`: Added `open System.Threading` + retry loop (3 attempts, 50ms/100ms delays)
- `src/Fable.Cli/CHANGELOG.md`: Added `* [All] Fix watch mode stopping when a source file is briefly locked by another process (fixes #4080)`
- `src/Fable.Compiler/CHANGELOG.md`: Same entry

### PR description:
Title: `[Repo Assist] [All] Fix watch mode stopping when source file is briefly locked by editor`
Closes #4080. Root cause: editors that do atomic saves briefly lock files, causing IOException in readAllTextNonBlocking. Fix: retry up to 3 times with 50ms/100ms delays.

### File.fs change (key part):
```fsharp
let rec tryRead attempt =
    try
        use fileStream =
            new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use textReader = new StreamReader(fileStream)
        textReader.ReadToEnd()
    with :? IOException when attempt < 3 ->
        Thread.Sleep(if attempt = 1 then 50 else 100)
        tryRead (attempt + 1)
```
NOTE: Must add `open System.Threading` and `open System.IO` (IO already present).

## Comment to Post on #2110 (IsUnion false for Option<int>)

🤖 *This is an automated response from Repo Assist.*

This issue is addressed by draft PR #4529: **Fix FSharpOption not recognized as union type in F# reflection**.

The root cause is that `option_type()` in `fable-library-ts/Reflection.ts` and `fable-library-py/fable_library/reflection.py` creates a `TypeInfo` without a `cases` property, so:
- `FSharpType.IsUnion(typeof<option<int>>)` returns `false`
- `FSharpType.GetUnionCases(typeof<option<int>>)` throws
- `FSharpValue.GetUnionFields` and `MakeUnion` also fail for option types

PR #4529 rewrites `option_type()` to expose `None` (tag 0) and `Some` (tag 1) as union cases, and adds option-aware handling in `getUnionFields`/`makeUnion` to handle Fable's erased runtime representation (None = undefined/null, Some(x) = x directly).

## Comment to Post on #4224 (JSX match case children)

🤖 *This is an automated response from Repo Assist.*

**Root Cause**: The JSX `Unroller` in `src/Fable.Transforms/Fable2Babel.fs` handles simple expressions when lifting children out of JSX props. However, when a `match` expression has multiple branches with `for` loops or sequences, the F# compiler generates a block-body lambda (an `IEnumerable<ReactElement>` accumulator pattern). The `Unroller` doesn't recognise this pattern and leaves the unevaluated lambda as a JSX child — React then errors with "Functions are not valid as a React child."

**Workaround**: Compute children in a `let` binding before the `JSX.create` call:
```fsharp
let items = [
    match counter with
    | 0 -> Html.li "No items!"
    | n ->
        for i in 1..n do
            Html.li [ prop.key i; prop.text $"Item {i}" ]
]
Fable.Core.JSX.create "ul" [ "children" ==> items ]
```

**Fix location**: The `Unroller` active pattern in `Fable2Babel.fs` needs a new case to eagerly evaluate block-body lambdas used as JSX children.

## Monthly Issue #4471 Update

The monthly issue needs to be updated with the current run entry. Open Repo Assist PRs (13 total):
- #4533: [TypeScript] Fix static class members not re-declaring class-level type params (fixes #3504)
- #4532: [All] Emit compile-time error when inline function references private value (fixes #3866)
- #4529: Fix FSharpOption not recognized as union type in F# reflection (fixes #4082, #2110)
- #4525: [Eng] Add NuGet and npm package caching to CI workflow
- #4521: [JS/TS] Fix DateTimeOffset.ToString using local timezone instead of stored offset
- #4495: Add TypeScript output support and snapshot update mode to integration tests
- #4487: [JS/TS] Fix source maps dropping valid mappings at column 0
- #4465: [JS/TS] Add Async.AwaitEvent support
- #4451: [JS/TS] Fix String.IndexOf/LastIndexOf ignoring StringComparison argument
- #4450: [JS/TS] Add regression tests for N0, C0, P0 format specifiers
- #4417: [JS/TS] Fix implicit DateTime→DateTimeOffset conversion emitting missing type
- #4414: [JS/TS] Fix super call in generic class hierarchy using wrong mangled name
- #4411: [JS/TS] Fix float32 arithmetic producing float64 results

### SAFEOUTPUTS BLOCKER
The safeoutputs MCP is blocked by policy (MCP registry returns 401 from api.github.com/copilot/mcp_registry). This has been happening since at least 2026-04-15. This is an infrastructure issue outside our control. When this is fixed, execute all the PENDING actions above.
