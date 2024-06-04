# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

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
