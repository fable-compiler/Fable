---
last_commit_released: 1b67f038a33c0d515441104bde30e62746c477b4
include:
  - ../Fable.Core/
  - ../fable-standalone/
  - ../Fable.Transforms/
---

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.0.1 - 2026-04-29

### 🐞 Bug Fixes

* *(js/ts)* Fix JSX props with long string values causing compile error (fixes #3839) (#4545) ([d828a46](https://github.com/fable-compiler/Fable/commit/d828a461797e3f33bf4ab99b46030d16b29771e6))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/b471dc16fc3b5132af77b5974d1669c9b8220cca..1b67f038a33c0d515441104bde30e62746c477b4)</small></strong>

## 2.0.0 - 2025-04-21

* Fable 5

## 2.0.0-rc.1 - 2025-02-26

* Fable 5.0.0-rc.1

## 2.0.0-beta.1 - 2025-02-16

* Fable 5.0.0-alpha.10
* Replace `FABLE_COMPILER_4` with `FABLE_COMPILER_5` as the compiler directive

## 1.2.2 - 2024-05-24

### Fixed

* Fixed includes to come from this package internals (by @MangelMaxime)

## 1.2.1 - 2024-05-24

### Fixed

* Fixed includes to come from this package internals and to use `fable-library-js` instead of `fable-library-ts` (by @ncave)

## 1.2.0 - 2024-05-23

### Changed

* Use `@fable-org/fable-metadata` package instead of `fable-metadata` (by @MangelMaxime)
* Use `@fable-org/fable-standalone` package instead of `fable-standalone` (by @MangelMaxime)
* Make `GetDirectoryName` return `"."` instead of `""` if the path doesn't contain any directory (by @MangelMaxime)

### Fixed

* Fix initialization of `fable-standalone` with the new package format (by @MangelMaxime)

## 1.1.0 - 2024-02-20

* Add `NPM_PACKAGE_FABLE_COMPILER_JAVASCRIPT` compiler directive (by @MangelMaxime)

## 1.0.0 - 2024-02-12

* Release stable version

## 1.0.0-beta-001 - 2024-02-12

### Added

* First release as part of `@fable-org` scope
