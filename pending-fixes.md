# Pending Fixes and Actions

## DONE THIS RUN: DateTime format specifiers fix

**Branch**: `repo-assist/fix-datetime-format-specifiers-20260419`
**Status**: COMMITTED + PR CREATED (via safeoutputs, awaiting PR number assignment)
**Issues fixed**: #3976 — Missing DateTime format specifiers (R, s, u, F, f, G, g, M, U, Y)
**Fix**: Added helper functions and switch cases for all missing standard specifiers in:
- `src/fable-library-ts/Date.ts`: added `dateToString_R/s/u/M/Y` helpers
- `src/fable-library-py/fable_library/date.py`: added `to_rfc1123_string/to_sortable_string/etc` helpers
- Tests in `tests/Js/Main/DateTimeTests.fs` and `tests/Python/TestDateTime.fs`

## DONE PREV RUN: #3839 JSX long string fix → PR #4545

## printf %g trailing zeros fix → PR #4543

## Open Repo Assist PRs (16 total)

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
- #4545: [JS/TS] Fix JSX props > 100 chars (closes #3839)
- #4546: [Eng] Add CI concurrency settings
- branch `repo-assist/fix-datetime-format-specifiers-20260419`: [JS/TS/Python] Fix missing DateTime format specifiers (PR created this run)

## Future investigation targets

- #2654: G-format exponent notation bug (trimEnd strips trailing zero from exponent) — root cause posted
- #4224: JSX match-case children (Unroller in Fable2Babel.fs needs new case)
- #3919: importValueDynamic multi-arg (needs arity-aware lambda wrapping in Replacements.fs)
- #3853: Erased union case
- #3861: jsOptions/copyOfStruct (root cause posted, fix is non-trivial)

