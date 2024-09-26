# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Fixed

* [GH-3900](https://github.com/fable-compiler/Fable/pull/3900) [Python] Fix nuget packages with hypens in their names  (by @MangelMaxime)

## 4.0.0-alpha-013 - 2024-09-19

### Changed

* [All] Don't hide original error when failing to scan an assembly for plugin (#3896) (by @MangelMaxime)

## 4.0.0-alpha-012 - 2024-06-17

### Fixed

* [JS/TS] Fixed unwrapping optional arguments (#3847) (by @ncave)

## 4.0.0-alpha-011 - 2024-06-13

### Changed

* Fable 4.19.1

## 4.0.0-alpha-010 - 2024-05-24

### Fixed

* [GH-3835](https://github.com/fable-compiler/Fable/pull/3835) [All] Use case insensitive comparison when searching for dependencies between fable packages (by @ThisFunctionalTom)

## 4.0.0-alpha-009 - 2024-05-24

### Changed

* Fable 4.18.0

## 4.0.0-alpha-008 - 2024-02-27

### Fixed

* [GH-3769](https://github.com/fable-compiler/Fable/pull/3769) The dependent files of the current file should be detected for the signature file if there is one present. (by @nojaf)
* [GH-3769](https://github.com/fable-compiler/Fable/pull/3769) Local plugin build does not run indefinably. (by @nojaf)

## 4.0.0-alpha-007 - 2024-02-20

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
