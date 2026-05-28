---
last_commit_released: 63bcd3d90f37cb3934edcc59b5f54f49ffab3896
include:
  - ../Fable.Core/
  - ../fable-standalone/
  - ../Fable.Transforms/
---

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.0.1 - 2026-05-28

### 🐞 Bug Fixes

* [JS/TS/Tests] Remove mocha to resolve package vulnerabilities (#4604) ([092324b](https://github.com/fable-compiler/Fable/commit/092324b55d17973ef8364c00d4ed48eb067c0f23))
* [Rust] Update hashing and datetime tests (#4613) ([9d39037](https://github.com/fable-compiler/Fable/commit/9d390376fd2c81f76fde70c522b59f61a9c9175d))
* *(all)* Duplicate LetRec bindings during inline expansion (#4592) ([62612a5](https://github.com/fable-compiler/Fable/commit/62612a5bb42644934b7573b7cf8f9db930c5dc37))
* *(all)* Fix super call in multi-level generic class hierarchy using wrong mangled name (#4414) ([db1d4d2](https://github.com/fable-compiler/Fable/commit/db1d4d2e8cdbb9d5a16856c91192c1f0fab3f783))
* *(js/ts)* Fix JSX props with long string values causing compile error (fixes #3839) (#4545) ([d828a46](https://github.com/fable-compiler/Fable/commit/d828a461797e3f33bf4ab99b46030d16b29771e6))
* *(js/ts)* Add support for `match` clauses in `JSX.create` (#4620) ([616edca](https://github.com/fable-compiler/Fable/commit/616edca568890549ff81cfa18ab32ca0b807c8eb))
* *(js/ts)* Don't spread last arg in String.Concat call (#4621) ([3de580e](https://github.com/fable-compiler/Fable/commit/3de580e4447ed0090a79648caa8f1e5003bac91d))
* *(js/ts)* Inline AttachMembers members at call sites (#4622) ([63bcd3d](https://github.com/fable-compiler/Fable/commit/63bcd3d90f37cb3934edcc59b5f54f49ffab3896))
* *(python)* Avoid duplicate captured argument in hoisted guards (#4610) (#4611) ([90aec48](https://github.com/fable-compiler/Fable/commit/90aec483296d26549aedf9eaf46f0825d70a4022))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/b471dc16fc3b5132af77b5974d1669c9b8220cca..63bcd3d90f37cb3934edcc59b5f54f49ffab3896)</small></strong>

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
