---
last_commit_released: 5ee8bbb832d9677ad702104ded8e00c09619393f
updaters:
  - package.json:
      file: package.json
  - regex:
      file: ../fable-compiler-js/package.json
      pattern: '(?<="@fable-org/fable-metadata": "\^)[^"]+'
---

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.2.0 - 2026-10-21

### 🚀 Features

* *(js/ts)* Add CaseTest attribute for TypeScriptTaggedUnion cases that share a tag (#5076) ([5ee8bbb8](https://github.com/fable-compiler/Fable/commit/5ee8bbb832d9677ad702104ded8e00c09619393f))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/a8661874d60694f0faad235adb7f760fed6f72a5..5ee8bbb832d9677ad702104ded8e00c09619393f)</small></strong>

## 2.1.1 - 2026-10-12

### 🐞 Bug Fixes

* *(all)* Support implicit conversion to erased unions (#5058) ([5d15141e](https://github.com/fable-compiler/Fable/commit/5d15141e519c2f320ac3457805b2d157cbea086f))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c977d78b39225a51c7bd051a1fe363ed0ccbe201..a8661874d60694f0faad235adb7f760fed6f72a5)</small></strong>

## 2.1.0 - 2026-06-30

### 🚀 Features

* *(all)* Add `Compiler.is*` target detection flags for conditional branching (#4692) ([621dea31](https://github.com/fable-compiler/Fable/commit/621dea31daaef6a9dcf8a2ac5ceb144a063bb873))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/b471dc16fc3b5132af77b5974d1669c9b8220cca..c977d78b39225a51c7bd051a1fe363ed0ccbe201)</small></strong>

## 2.0.0 - 2026-04-21

## 2.0.0-rc.1 - 2026-02-26

## 2.0.0-beta.2 - 2025-03-03

* Update `Fable.Core.dll` to support `CaseRules.LowerAll` (by @MangelMaxime)

## 2.0.0-beta.1 - 2025-02-16

* F# 9 support

## 1.1.0 - 2024-05-23

### Fixed

* Make `dirname-filename-esm` a `dependency` instead of a `devDependency`

## 1.0.0 - 2024-02-12

* Release stable version

## 1.0.0-beta-001 - 2024-02-12

### Added

* First release as part of `@fable-org` scope
