# Open Repo Assist PRs (as of 2026-04-27)

- #4553: fix(js): handle .NET format specifiers in F# interpolated strings
- #4547: Add missing standard DateTime format specifiers for JS/TS and Python
- #4546: [Eng] Add CI concurrency settings to cancel superseded PR runs
- #4545: [JS/TS] Fix JSX props with long string values causing compile error
- #4543: [JS/TS] Fix sprintf %g/%G trailing zeros (dirty - needs rebase)
- #4533: [TypeScript] Fix static class members not re-declaring class-level type params
- #4532: [All] Emit compile-time error when inline function references private value
- #4529: Fix FSharpOption not recognized as union type in F# reflection
- #4525: [Eng] Add NuGet and npm package caching to CI workflow
- #4521: [JS/TS] Fix DateTimeOffset.ToString using local timezone
- #4495: Add TypeScript output support and snapshot update mode to integration tests
- #4487: [JS/TS] Fix source maps dropping valid mappings at column 0
- #4465: [JS/TS] Add Async.AwaitEvent support
- #4451: [JS/TS] Fix String.IndexOf/LastIndexOf ignoring StringComparison
- #4450: [JS/TS] Add regression tests for N0, C0, P0 format specifiers
- #4417: [JS/TS] Fix implicit DateTime→DateTimeOffset conversion
- #4414: [JS/TS] Fix super call in generic class hierarchy using wrong mangled name
- #4411: [JS/TS] Fix float32 arithmetic producing float64 results
- NEW: fix(python): fix regex lookbehind patterns being incorrectly converted (fixes #3918) - PR # TBD

# Future Investigation Targets
- #2654 G17 format: trimEnd("0") corrupts exponent (e.g. 1e-10) - root cause known, PR #4543 partially helps
- #4224 JSX match-case children bug (Unroller in Fable2Babel.fs needs new case)
- #3919 importValueDynamic multi-arg functions (arity-aware lambda wrapping needed)
- Python/Dart/Beam .NET format specifiers in interpolated strings (follow-up to #4553)
