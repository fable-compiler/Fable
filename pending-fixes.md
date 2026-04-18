# Pending Fixes and Actions

## URGENT: Create PR for #3839 JSX long string fix (this run — NOT DONE)

**Branch**: `repo-assist/fix-issue-3839-jsx-long-string-prop`
**Status**: COMMITTED. PR creation FAILED (safeoutputs MCP session expired). MUST CREATE PR NEXT RUN.
**Issue**: #3839 — JSX props with string values > 100 chars cause "Cannot detect JSX prop key at compile time"
**Files changed**:
- `src/Fable.Transforms/Fable2Babel.fs`: Added OR pattern in `transformJsxProps` to handle Let-wrapped props
- `tests/Integration/Integration/data/jsxListOptimisation/Components.fs`: Added `divWithLongClassName` test
- `tests/Integration/Integration/data/jsxListOptimisation/Components.jsx.expected`: Added expected output
- Changelogs updated
**Action**: Create PR with title "[JS/TS] Fix JSX props with string values longer than 100 chars causing compile error"
**Also**: Comment on #3839 after PR is created

## URGENT: Comment on #3861 (this run — NOT DONE)

Root cause analysis ready: For `abstract fn: int -> int` (MemberKind.Member), F# compiles `o.fn <- value` using a byref/FSharpRef pattern that creates a temp `copyOfStruct`. Fable's `makePojoFromLambda`/`groupByGetter` doesn't recognize the Let-wrapped byref, falls back to lambda, and JS references `copyOfStruct` undeclared. Workaround: `abstract fn: (int -> int) with get, set`.

## URGENT: Update Monthly Summary #4471 (this run — NOT DONE)

Add run entry for 2026-04-18 (run ID 24593151387):
- 🔧 Created branch for PR (fix #3839): [JS/TS] Fix JSX props with string values > 100 chars
- 💬 (attempted) Comment on #3861: root cause analysis for jsOptions/copyOfStruct bug
- Note: safeoutputs session expired, no GitHub writes completed

## printf %g trailing zeros fix

**Branch**: `repo-assist/fix-printf-g-trailing-zeros`
**PR**: #4543
**Status**: PR created (draft, open)

## #4080 Watch Mode File Lock (ABANDONED)

Branch was lost between runs and re-implementation is non-trivial. Defer.

## Open Repo Assist PRs (15 total)

- #4411: [JS/TS] Fix float32 arithmetic producing float64 results
- #4414: [JS/TS] Fix super call in generic class hierarchy using wrong mangled name
- #4417: [JS/TS] Fix implicit DateTime→DateTimeOffset conversion
- #4450: [JS/TS] Add regression tests for N0, C0, P0 format specifiers
- #4451: [JS/TS] Fix String.IndexOf/LastIndexOf ignoring StringComparison argument
- #4465: [JS/TS] Add Async.AwaitEvent support (closes #4239)
- #4487: [JS/TS] Fix source maps dropping valid mappings at column 0
- #4495: Add TypeScript output support and snapshot update mode to integration tests
- #4521: [JS/TS] Fix DateTimeOffset.ToString using local timezone
- #4525: [Eng] Add NuGet and npm package caching to CI workflow
- #4529: Fix FSharpOption not recognized as union type in F# reflection (closes #4082)
- #4532: [All] Emit compile-time error when inline function references private value (closes #3866)
- #4533: [TypeScript] Fix static class members not re-declaring class-level type parameters (closes #3504)
- #4543: [JS/TS] Fix sprintf %g/%G not stripping trailing zeros when explicit precision is given
- branch `repo-assist/fix-issue-3839-jsx-long-string-prop`: [JS/TS] Fix JSX props > 100 chars (NO PR YET - create next run)

## Future investigation targets

- #4224: JSX match-case children (Unroller in Fable2Babel.fs needs new case)
- #3919: importValueDynamic multi-arg (needs arity-aware lambda wrapping in Replacements.fs)
- #3976: Missing DateTime format specifiers
- #3853: Erased union case
- #3861: jsOptions/copyOfStruct (non-trivial fix, comment posted)
