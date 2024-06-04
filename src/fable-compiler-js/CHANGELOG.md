# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

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
