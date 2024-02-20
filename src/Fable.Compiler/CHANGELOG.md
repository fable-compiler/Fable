# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Added

* [GH-3758](https://github.com/fable-compiler/Fable/pull/3758) Endpoint to get Fable.AST for a file. (by @nojaf)

### Fixed

* [GH-3763](https://github.com/fable-compiler/Fable/pull/3763) Redirect plugin build stdout and pass to logger. (by @nojaf)

## 4.0.0-alpha-006 - 2024-02-12

### Changed

* Update to Fable.AST 4.4.0

## 4.0.0-alpha-005 - 2024-02-12

### Changed

* [GH-3742](https://github.com/fable-compiler/Fable/pull/3742) Return diagnostics in compile response (by @nojaf)
* [GH-3746](https://github.com/fable-compiler/Fable/pull/3746) Extract type-checking from compilation (by @nojaf)

## 4.0.0-alpha-004 - 2024-01-30

### Changed

* [GH-3724](https://github.com/fable-compiler/Fable/pull/3724) Internalize `GetFableModulesFromDir` &amp; `GetFableModulesFromProject` (by @nojaf)

## 4.0.0-alpha-003 - 2024-01-30

* [GH-3720](https://github.com/fable-compiler/Fable/pull/3720) Introduce `ProjectCrackerResolver` allowing customizing the project resolution (by @nojaf)

## 4.0.0-alpha-002 - 2024-01-25

### Changed

* Respect file extension from `CliArgs`
* Use `Microsoft.Extensions.Logging`
* Load Fable plugins

## 4.0.0-alpha-001 - 2023-12-14

* Initial release

## 4.0.0-beta-001 - 2023-12-14 [YANKED]

* Initial release
