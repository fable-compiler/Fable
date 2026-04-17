# Pending Fixes and Actions

## printf %g trailing zeros fix (this run)

**Branch**: `repo-assist/fix-printf-g-trailing-zeros`
**Status**: Committed and PR created (no PR number yet)
**Files changed**:
- `src/fable-library-ts/String.ts`: Added trailing zero stripping to `%g`/`%G` cases in `formatReplacement()` and improved scientific notation handling in `format()` G case
- `tests/Js/Main/StringTests.fs`: Added test `sprintf %g strips trailing zeros`
- Changelogs updated

## #4080 Watch Mode File Lock (ABANDONED)

Branch was lost between runs and re-implementation is non-trivial. Defer.

## Comment Backlog (DONE)

- ✅ #2110: Commented (PR #4529 fixes this)
- ✅ #4224: Commented (root cause + workaround)
- ✅ #3919: Commented (importValueDynamic root cause analysis)

## Open Repo Assist PRs (14 total)

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
- branch `repo-assist/fix-printf-g-trailing-zeros`: [JS/TS] Fix sprintf %g trailing zeros (no PR# yet)

## Future investigation targets

- #4224: JSX match-case children (Unroller in Fable2Babel.fs needs new case)
- #3919: importValueDynamic multi-arg (needs arity-aware lambda wrapping in Replacements.fs)
- #3976: Missing DateTime format specifiers
- #3853: Erased union case
