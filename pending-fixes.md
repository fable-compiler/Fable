# Pending Fixes and Actions

## DONE THIS RUN: #3839 JSX long string fix

**Branch**: `repo-assist/fix-issue-3839-jsx-long-string-prop-v2`
**Status**: COMMITTED + PR CREATED (via safeoutputs, awaiting PR number assignment)
**Issue**: #3839 — JSX props with string values > 100 chars cause "Cannot detect JSX prop key at compile time"
**Fix**: Added `MaybeCasted(Fable.Let(_, letValue, MaybeCasted(Fable.Value(Fable.NewTuple([ StringConst key; _ ], _), _))))` match arm in `transformJsxProps`

## DONE THIS RUN: Comment on #3861

Root cause posted: `groupByGetter` in `makePojoFromLambda` (Replacements.fs) only handles FieldSet on IdentExpr/Get patterns. `abstract fn: int -> int` (MemberKind.Member) generates byref/FSharpRef assignment pattern which doesn't match, silently ignored, then copyOfStruct referenced but never declared. Workaround: use `with get, set`.

## printf %g trailing zeros fix

**Branch**: `repo-assist/fix-printf-g-trailing-zeros-80d5cf0dc636481e`
**PR**: #4543
**Status**: PR created (draft, open)

## #4080 Watch Mode File Lock (ABANDONED)

Branch was lost between runs and re-implementation is non-trivial. Defer.

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
- #4543: [JS/TS] Fix sprintf %g/%G not stripping trailing zeros when explicit precision is given
- branch `repo-assist/fix-issue-3839-jsx-long-string-prop-v2`: [JS/TS] Fix JSX props > 100 chars (PR created this run)

## Future investigation targets

- #4224: JSX match-case children (Unroller in Fable2Babel.fs needs new case)
- #3919: importValueDynamic multi-arg (needs arity-aware lambda wrapping in Replacements.fs)
- #3976: Missing DateTime format specifiers
- #3853: Erased union case
- #3861: jsOptions/copyOfStruct (root cause posted, fix is non-trivial)
