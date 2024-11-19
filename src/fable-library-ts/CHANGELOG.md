# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

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
* [JS/TS] Fix TypeScript compilation by resolving type of `jsOptions` (#3894) (by @ManngelMaxime)

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
