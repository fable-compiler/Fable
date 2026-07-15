---
last_commit_released: 6568f35ece227fc30ce84111c8d2e975b09bac00
updaters:
  - package.json:
      file: package.json
  - regex:
      file: ./../Fable.Transforms/Global/Compiler.fs
      pattern: (?<=let JS_LIBRARY_VERSION = ").*(?=")
---

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.5.0 - 2026-07-15

### 🚀 Features

* *(js/ts)* Add `System.IO.Directory` support (Exists, CreateDirectory) (#4798) ([6568f35e](https://github.com/fable-compiler/Fable/commit/6568f35ece227fc30ce84111c8d2e975b09bac00))
* *(js/ts/python/beam)* Preserve member calls and pattern matches in quotations (#4780) ([55482e6b](https://github.com/fable-compiler/Fable/commit/55482e6b5e6306fd4dbaba5759024313cb26682a))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/a8d9bf5f0b7cc4560bb107963348e765695c22b6..6568f35ece227fc30ce84111c8d2e975b09bac00)</small></strong>

## 2.4.2 - 2026-07-11

### 🐞 Bug Fixes

* *(js/ts)* Async cancellation propagation and continuation double-resolve (#4746) ([c78a9a8e](https://github.com/fable-compiler/Fable/commit/c78a9a8ed20234feddf3375b61e91098f9261bee))
* *(js/ts)* Date arithmetic: PM parsing, AddMonths day clamp, AddYears offset (#4744) ([be0558eb](https://github.com/fable-compiler/Fable/commit/be0558ebe07f78f7a93ec0c34592d7c066b59fc7))
* *(js/ts)* Int64 non-decimal parsing and BitConverter .NET mismatches (#4748) ([2704d9c5](https://github.com/fable-compiler/Fable/commit/2704d9c5daab87eb1791c0ea1b6b8b06b987247c))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/d6ae6bd3790b57b31941a118cdffaeb6a59155c3..a8d9bf5f0b7cc4560bb107963348e765695c22b6)</small></strong>

## 2.4.1 - 2026-07-09

### 🐞 Bug Fixes

* MailboxProcessor dropping falsy messages and unit replies ([8299b2e7](https://github.com/fable-compiler/Fable/commit/8299b2e7f690e9136906653155883a08607aa4a4))
* *(all)* Add `Seq.enumerateTryWith` for try/with in seq, list, array comprehensions (#4750) ([448c90d7](https://github.com/fable-compiler/Fable/commit/448c90d7ccba80ee3d3c38432a2a1d1407f6dc69))
* *(js/ts)* Match .NET NaN semantics in JS comparison, min and max ([7e5106f9](https://github.com/fable-compiler/Fable/commit/7e5106f9d93cb55ae4aca4656868fa0c53493991))
* *(js/ts)* BigInteger byte-array corruption, checked conversions, gcd, log2 (#4743) ([f3cb7f2b](https://github.com/fable-compiler/Fable/commit/f3cb7f2bfc9e46e558763867d6c1ba323bcf281c))
* *(js/ts)* Observable/Event dropping null/undefined/unit values (#4742) ([75bff73b](https://github.com/fable-compiler/Fable/commit/75bff73be3743fe0af37f99c541b10f6978150f3))
* *(js/ts)* EndsWith regression for empty pattern with non-ordinal comparison ([7793ed7b](https://github.com/fable-compiler/Fable/commit/7793ed7b0fc1f33bf3da6c0ea31607e172726267))
* *(js/ts/python)* Quote strings with the %A format specifier (#4749) ([75321f3b](https://github.com/fable-compiler/Fable/commit/75321f3b2cb70ea345a0a45d6343cbb6e0de466f))
* *(ts/js)* `Regex.Replace` count/substitutions and `Regex.Split` semantics (#4741) ([d6ae6bd3](https://github.com/fable-compiler/Fable/commit/d6ae6bd3790b57b31941a118cdffaeb6a59155c3))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/7f915f1dd66b9a5fbbd56f858b07d39b98519b65..d6ae6bd3790b57b31941a118cdffaeb6a59155c3)</small></strong>

## 2.4.0 - 2026-07-05

### 🚀 Features

* *(js/ts)* Add `System.IO.Path` and `System.IO.File` API support ([ec99fac5](https://github.com/fable-compiler/Fable/commit/ec99fac5fd7a9428ee54f66f878ef4edad0904cb))
* *(js/ts/python)* Add Environment and Console.Error support ([6e082f95](https://github.com/fable-compiler/Fable/commit/6e082f955140676da3388a43b55378ea4ad20f1c))

### 🐞 Bug Fixes

* *(js/ts)* DateTime.ToString() and %A are now consistent across all DateTimeKind values (#4714) ([de98b0e4](https://github.com/fable-compiler/Fable/commit/de98b0e466c8824b0ba8b941d20efae70d232abb))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c977d78b39225a51c7bd051a1fe363ed0ccbe201..7f915f1dd66b9a5fbbd56f858b07d39b98519b65)</small></strong>

## 2.3.0 - 2026-06-30

### 🚀 Features

* *(js/ts)* Map task { } to Promise<T> ([97f54d36](https://github.com/fable-compiler/Fable/commit/97f54d3692e5ba881c249b289c20b4fad5ac27e2))
* *(js/ts/python/beam)* Add support for `Async.AwaitEvent` (#4693) ([71c98179](https://github.com/fable-compiler/Fable/commit/71c981792b6ff99cd417bb2e70a13a9d55ed72cf))

### 🐞 Bug Fixes

* *(js)* Respect StringComparison in String.IndexOf/LastIndexOf (#4681) ([7cb92d52](https://github.com/fable-compiler/Fable/commit/7cb92d522e5dc8dfe51351565504c59a94c2f408))
* *(js/ts)* Int list is not a union type (#4698) ([ecbecbe7](https://github.com/fable-compiler/Fable/commit/ecbecbe756610fd83a621c005e97eda92bbc332a))
* *(js/ts)* Pass TypeInfo to getRecordFields to handle None fields in anonymous records (#4704) ([c977d78b](https://github.com/fable-compiler/Fable/commit/c977d78b39225a51c7bd051a1fe363ed0ccbe201))
* *(ts)* Widen isEnumDefined value parameter to accept any value (#4690) ([0d6498dd](https://github.com/fable-compiler/Fable/commit/0d6498ddf65e0e18918690633da23b04b78479f6))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c9b3ee2429a4688946c1936e27df730837428070..c977d78b39225a51c7bd051a1fe363ed0ccbe201)</small></strong>

## 2.2.0 - 2026-06-24

### 🚀 Features

* *(all)* Support Exception.InnerException across all targets (#4677) ([c9b3ee24](https://github.com/fable-compiler/Fable/commit/c9b3ee2429a4688946c1936e27df730837428070))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/15eb83ed36657f75073fb0e1b4cac677e24fc9bb..c9b3ee2429a4688946c1936e27df730837428070)</small></strong>

## 2.1.1 - 2026-06-10

### 🐞 Bug Fixes

* *(js/ts)* Reject JS-permissive date strings that .NET TryParse rejects (#4588) ([09003de](https://github.com/fable-compiler/Fable/commit/09003de8bc72e4f73281828d905d49aab7db3f55))
* *(js/ts)* Make Exception.ToString() return the message instead of "Exception" (#4635) ([4c36f76](https://github.com/fable-compiler/Fable/commit/4c36f761bd378da800e30657c58355bd5324a653))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/63bcd3d90f37cb3934edcc59b5f54f49ffab3896..15eb83ed36657f75073fb0e1b4cac677e24fc9bb)</small></strong>

## 2.1.0 - 2026-05-28

### 🚀 Features

* *(js/ts/python)* Add missing standard DateTime format specifiers for JS/TS and Python (#4547) ([90f5eb7](https://github.com/fable-compiler/Fable/commit/90f5eb7f61710bc679f6fdec30ff5d864984ac08))
* *(js/ts/python)* Add missing StringBuilder.Append overloads for numeric types (#4568) ([b402c30](https://github.com/fable-compiler/Fable/commit/b402c30d04bfca8644314ba4b21894abfff6a713))

### 🐞 Bug Fixes

* [JS/TS] Fix DatetimeOffset.ToString("s") (#4596) ([3ce6f3f](https://github.com/fable-compiler/Fable/commit/3ce6f3fd88cc7f67e34e2bbc311e722c55ffbf45))
* *(js/ts)* Fix datetime custom format off by one year (#4558) ([83bdbb5](https://github.com/fable-compiler/Fable/commit/83bdbb5b34e70eae203831a3a442d477d15911e1))
* *(js/ts)* Hex format specifier uses no padding unless precision is specified (#4603) ([ba9857a](https://github.com/fable-compiler/Fable/commit/ba9857a4dcbf7ab1f936241f6619fda43e039040))
* *(js/ts)* Fix Decimal.GetBits returning incorrect mantissa after round arithmetic result (#4561) ([345bcd4](https://github.com/fable-compiler/Fable/commit/345bcd4b99417b304c71059966126fad74cf3a97))
* *(js/ts)* Fix G/g format specifier corrupting exponential notation when trimming trailing zeros (#4587) ([773c098](https://github.com/fable-compiler/Fable/commit/773c098a8f7ac7e8db1d4e490dec1294a80321ee))
* *(js/ts/python)* Fix FSharpOption not recognized as union type in F# reflection (#4529) ([d78a37d](https://github.com/fable-compiler/Fable/commit/d78a37db9f4c25eb51fac8afcd320b4ea36c60a7))
* *(ts)* Enforce browser-only compatibility in fable-library-ts tsconfig (#4563) ([10c81c1](https://github.com/fable-compiler/Fable/commit/10c81c1361208eb61f5fa78c6704e6fccc068fb1))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/b471dc16fc3b5132af77b5974d1669c9b8220cca..63bcd3d90f37cb3934edcc59b5f54f49ffab3896)</small></strong>

## 2.0.0 - 2026-04-21

### Fixed

* [JS/TS] Fix `ResizeArray` (`System.Collections.Generic.List`) equality to use reference equality instead of structural equality (fixes #3718)
* [JS/TS] Fix `String.Contains` ignoring `StringComparison` argument (second argument was silently discarded)

## 2.0.0-rc.6 - 2026-04-07

### Fixed

* [JS/TS] Fix `Async.StartChild` with timeout always timing out even when the computation finishes before the deadline (fixes #4481) (by @MangelMaxime)

## 2.0.0-rc.5 - 2026-03-31

### Fixed

* [JS/TS] Improve `Regex.Escape` and `Regex.Unescape` handling (by @MangelMaxime)
* [JS/TS] Fixed quotation for union string cases (by @MangelMaxime)

## 2.0.0-rc.4 - 2026-03-19

### Fixed

* [JS/TS] Fix `Unchecked.defaultof<char>` being emitted as `null` instead of `'\0'` (by @MangelMaxime)

## 2.0.0-rc.3 - 2026-03-10

### Fixed

* [TS] Expose optional `stack` property on `Exception` (by @MangelMaxime)

## 2.0.0-rc.2 - 2026-03-03

### Fixed

* [JS/TS] Allows compiling `fable-library-ts` for Browser environment (by @goswinr)

## 2.0.0-rc.1 - 2026-02-26

## 2.0.0-beta.7 - 2026-02-03

* [JS/TS] Fix `StringBuilder.Chars` getter and setter (by @MangelMaxime)

## 2.0.0-beta.6 - 2025-12-26

### Fixed

* [JS/TS] Fix #4305 `DateTimeOffset.Now` returns wrong time (by @ncave)

## 2.0.0-beta.5 - 2025-11-19

### Changed

* [JS/TS] Replace the deprecated `substr` method with `slice` (by @Thorium)

## 2.0.0-beta.4 - 2025-07-25

### Added

* [JS/TS] Initial support for Nullable Reference Types (by @ncave)

## 2.0.0-beta.3 - 2025-03-14

### Fixed

* [JS/TS] Make `nullArgCheck` report the same error message as on .NET (by @MangelMaxime)

## 2.0.0-beta.2 - 2025-03-03

* [JS/TS] Fix #4049: decimal/bigint to integer conversion checks (by @ncave)
* [JS/TS] Fix `decimal` to `char` conversion checks (by @ManngelMaxime)
* [JS/TS] Propagate non-captured exception when running `Async.Start` or `Async.StartImmediate` (by @MangelMaxime)
* [JS/TS] Remove `Async.RunSynchronously` (by @MangelMaxime)
* [JS/TS] Change signature of `startWithContinuations` to always require all its arguments (by @MangelMaxime)
* [JS/TS] Fix short `DateTime` and `DateTimeOffset` short format strings (by @MangelMaxime)
* [JS/TS] Add `C` and `c` format for numeric types (by @MangelMaxime)
* [JS/TS] Add `B` and `b` format for numeric types (by @MangelMaxime)
* [JS/TS] Add `n` format for numeric types (by @MangelMaxime)
* [JS/TS] Fix numeric formats (by @MangelMaxime)

## 2.0.0-beta.1 - 2025-02-16

* Compiled with Fable 5.0.0-alpha.10

## 1.10.0 - 2025-01-23

### Added

* [JS/TS] Add `StringBuiler.Append(c: char, repeatCount: int)` overload (by @roboz0r)

## 1.9.0 - 2025-01-09

### Added

* [JS/TS] Add new `TimSpane` overload support coming from .NET 9 (by @MangelMaxime)

### Fixed

* [JS/TS] Fix `DateTimeOffset.ToLocalTime` (by @MangelMaxime)

## 1.8.0 - 2024-11-19

* [JS/TS] Fix `h` in `DateTime.ToString` (@MangelMaxime)
* [JS/TS] Fix `hh` in `DateTime.ToString` (@MangelMaxime)

## 1.7.0 - 2024-11-19

### Fixed

* [JS/TS] Added missing IReadOnlyCollection helpers (#3953)

## 1.6.0 - 2024-10-02

### Removed

* Remove `Async` (from `Async.ts`) class (by @MangelMaxime)

### Changed

* Renamed `IAsync` to `Async` in `AsyncBuilder.ts` (#3906) (by @ncave)

## 1.5.0 - 2024-09-19

### Added

* [JS/TS] Add support for `OrdinalIgnoreCase` overload for `String.EndsWith` (#3892) (by @goswinr)
* [JS/TS] Add `uri.Port`, `uri.IsDefaultPort` (by @MangelMaxime)

### Fixed

* [JS/TS] Fix escaping of `{` and `}` in FormattableString (#3890) (by @roboz0r)
* [JS/TS] Fix `uri.Host` to return the host name without the port (by @MangelMaxime)
* [JS/TS] Fix TypeScript compilation by resolving type of `jsOptions` (#3894) (by @MangelMaxime)

## 1.4.3 - 2024-09-04

* [JS/TS] Fixed Decimal comparisons (#3884) (by @ncave)

## 1.4.2 - 2024-06-13

### Fixed

* [JS/TS] Fixed BigInt.ToDecimal with negative values (#3500) (by @ncave)

## 1.4.1 - 2024-06-13

### Fixed

* [JS/TS] Fixed DateTime.MinValue, DateTime.MaxValue (#3836) (by @ncave)

## 1.4.0 - 2024-03-20

### Added

* [JS/TS] Add `ConditionalWeakTable` (by @chkn)

## 1.3.0 - 2024-03-18

* [JS/TS] `Boolean.tryParse` should not crash on `null` string (@goswinr)

## 1.2.0 - 2024-03-01

### Fixed

* [GH-3772](https://github.com/fable-compiler/Fable/pull/3772) [JS/TS] Re-implement `DateTime.ToString` custom format handling (by @MangelMaxime)

    It now supports all custom format specifiers, and behave as if `CultureInfo.InvariantCulture` was used (Fable does not support Globalization).

## 1.1.0 - 2024-02-20

### Added

#### JavaScript

* [GH-3759](https://github.com/fable-compiler/Fable/issues/3759) Add `StringBuilder.Chars` (by @MangelMaxime)
* Add `StringBuilder.AppendFormat` (by @ncave)
* [GH-3748](https://github.com/fable-compiler/Fable/pull/3748) Add `Array.getItem` and `Array.setItem` (by @MangelMaxime)

## 1.0.0 - 2024-02-13

* Release stable version

## 1.0.0-beta-001 - 2024-02-12

### Changed

* Separate `Result` from `Choice`
* Released as part of Fable 4.12.0
