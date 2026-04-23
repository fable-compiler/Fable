# Pending Fixes and Actions

## Open Repo Assist PRs (18 total)

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
- #4547: [JS/TS/Python] Add missing DateTime format specifiers (R, s, u, F, f, G, g, M, U, Y) — fixes #3976
- #4553: [JS] Fix .NET format specifiers in F# interpolated strings (closes #4046 items 3+4)

## Notes on blocked engineering work

- setup-uv CI change: blocked by protected file policy (safeoutputs cannot create PRs for .github/workflows/ changes)
- Branch `repo-assist/eng-setup-uv-action-20260421` was committed locally but PR was never created successfully

## Future investigation targets

- #2654: G-format exponent notation bug (trimEnd strips trailing zero from exponent) — root cause posted
- #4224: JSX match-case children (Unroller in Fable2Babel.fs needs new case)
- #3919: importValueDynamic multi-arg (needs arity-aware lambda wrapping in Replacements.fs)
- #3853: Erased union case
- #3861: jsOptions/copyOfStruct (root cause posted, fix is non-trivial)
- Fix Python/Dart/Beam .NET format specifiers in interpolated strings (follow-up to #4553)

## Issues suggested for closing

- #4527: --noRestore bug fixed by merged PR #4548 (commented 2026-04-23)
- #4368: Buffer browser-compat fix already in Encoding.ts (commented 2026-04-23)
