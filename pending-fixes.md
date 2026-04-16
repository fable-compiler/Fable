# Pending Fixes and Actions

## Push PR #4529 (rebased - merge conflict resolved)

**Branch (local)**: `repo-assist/fix-issue-4082-option-union-reflection-7d1e11c14993f393`
**Commit**: `882547c64`
**Status**: Ready to force-push to origin (rebased onto main, conflicts resolved in changelogs)
**PR #4529** shows as "dirty" - force push will fix the merge conflict
**Action needed**: `push_to_pull_request_branch` for PR #4529 OR force push via git

## Create PR for #4080 (Watch mode file lock - already committed)

**Branch (local)**: `repo-assist/fix-issue-4080-watch-file-lock`
**Commit**: `364c113b1`
**Status**: Ready to push and create PR

### Changes made:
- `src/Fable.Compiler/File.fs`: Added `open System.Threading` + retry loop with 50ms/100ms sleeps
- `src/Fable.Cli/CHANGELOG.md`: Added entry
- `src/Fable.Compiler/CHANGELOG.md`: Added entry

### PR description:
Title: `[Repo Assist] [All] Fix watch mode stopping when file is briefly locked by editor`
Closes #4080. Root cause: editors that do atomic saves briefly lock files, causing IOException in readAllTextNonBlocking. Fix: retry up to 3 times with 50ms/100ms delays.

## Comment to Post on #2110 (IsUnion false for Option<int>)

🤖 *This is an automated response from Repo Assist.*

This issue is addressed by draft PR #4529: `option_type` is updated so `FSharpType.IsUnion typeof<option<int>>` returns `true`, and `FSharpValue.GetUnionFields` / `FSharpValue.MakeUnion` are updated to handle option's erased runtime representation correctly (JS: `None` = `undefined`, `Some x` via `some(x)`; Python: `None` = Python `None`, `Some` via `SomeWrapper`). A regression test has been added.

## Comment to Post on #4224 (JSX match case children)

🤖 *This is an automated response from Repo Assist.*

**Root Cause**: JSX `Unroller` in `src/Fable.Transforms/Fable2Babel.fs` (~line 2157) handles simple arrow functions, but when a `match` expression has complex branches (loops, sequences), it generates a block-body lambda that doesn't match any pattern. The unevaluated lambda ends up as a JSX child — React errors "Functions are not valid as a React child."

**Workaround**: Compute children before JSX:
```fsharp
let items = [
    match counter with
    | 0 -> Html.li "No items!"
    | n -> for i in 1..n do Html.li [prop.key i; prop.text $"Item {i}"]
]
Fable.Core.JSX.create "ul" ["children" ==> items]
```

**Fix location**: The `Unroller` active pattern needs a new case to handle block-body lambdas with complex control flow.
