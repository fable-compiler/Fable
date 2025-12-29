# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

### Added

* [Python] Add Python 3.10+ match statement support for pattern matching (by @dbrattli)

### Fixed

* [Python] Fix type annotations for curried functions and numeric types (by @dbrattli)
* [Python] Fix type annotations for inref, IList, DateKind, and regex collections (by @dbrattli)
* [Python] Fix type annotations for protocols, ABCs, Atom, and Set module (by @dbrattli)
* [Python] Fix type annotations for async functions, date operations, and None handling (by @dbrattli)
* [Python] Fix type annotations for tuple indexing, generic defaults, and reflection (by @dbrattli)

## 5.0.0-alpha.21 - 2025-12-26

### Fixed

* [Python] Use Fable AST for type parameter extraction instead of Python AST heuristics (by @dbrattli)
* [Python] Fix library type stubs for parse_int32/64 and from_integer (by @dbrattli)
* [Python] Fix missing type parameters on generic methods (by @dbrattli)
* [JS/TS] Fix #4305 DateTimeOffset.Now returns wrong time (by @ncave)

## 5.0.0-alpha.20 - 2025-12-15

### Added

* [Python] Support catching Python `BaseException` subclasses (`KeyboardInterrupt`, `SystemExit`, `GeneratorExit`) for Python interop (by @dbrattli)

### Changed

* [Python] F# `task { }` expressions now generate Python `async def` functions (by @dbrattli)
* [Python] Generate idiomatic `except` clauses for typed exception patterns (by @dbrattli)

### Fixed

* [Python] Fix `ResizeArray` compatibility with `Seq`/`Array` functions (by @dbrattli)
* [Python] Fix `FSharpList` generic type parameter handling for `IEnumerable_1` compatibility (by @dbrattli)

## 5.0.0-alpha.19 - 2025-12-08

### Added

* [Python] Add `Array.skipWhile` support (by @dbrattli)
* [Python] Add `Array.takeWhile` support (by @dbrattli)
* [Python] Allow `IEnumerator_1` as base class to fix typing issues (by @dbrattli)
* [Python] Add Pythonic import path syntax for relative imports (`.module`, `..parent`, `...grandparent`) (by @dbrattli)
* [Python] Add `[<Py.DecorateTemplate>]` attribute for creating custom decorator attributes (by @dbrattli)
* [Python] Add `[<Py.ClassAttributesTemplate>]` attribute for creating custom class attribute shortcuts (by @dbrattli)
* [Python] Add `[<Py.DataClass>]` as a built-in shorthand for `[<Py.ClassAttributes(style = Attributes, init = false)>]` (by @dbrattli)

### Fixed

* [Python] Fix String.Concat spread operator issue in Python transpilation (by @dbrattli)
* [Python] Fix regression `[<Erase>]` on class types not preventing them from being emitted to Python (by @dbrattli)
* [Python] Fix regression `%A` format specifier to output booleans as lowercase `true`/`false` (by @dbrattli)
* [Python] Fix various bugs in fable-library numeric types and string operations (by @dbrattli)

### Changed

* [Python] `[<Py.Decorate>]` now emits decorator strings verbatim and adds `importFrom` parameter for explicit import control (by @dbrattli)

## 5.0.0-alpha.18 - 2025-12-04

### Fixed

* [Python] Fix `printf.cont()` not applying continuation function when currying (by @dbrattli)

### Added

* [Python] Add support for `[<Py.Decorate>]` attribute on methods (previously only worked on classes) (by @dbrattli)
* [Python] Add new `[<Py.ClassMethod>]` attribute to emit @classmethod instead of @staticmethod (by @dbrattli)
* [Python] Added support for Pydantic serialization of core numeric and array types (by @dbrattli)

## 5.0.0-alpha.17 - 2025-12-03

### Fixed

* [Python] Fix regression, record member method naming (by @dbrattli)
* [Python] Fix regression, named arguments not being converted to snake_case (by @dbrattli)
* [Python] Fix regression, erased interfaces should not generate code (#4277) (by @dbrattli)

### Changed

* [Rust] Update `fable-library-rust` dependencies (by @ncave)
* [All] Update TargetFramework to net10.0 (by @ncave)
* [All] Update FCS to latest (commit [cfda5f6](https://github.com/dotnet/fsharp/commits/cfda5f68f781f1b336b7d6c1689d69f2c34da751/)) (by @ncave)

## 5.0.0-alpha.16 - 2025-11-27

### Fixed

* [All] Fix quoted paths resolution by `Util.getExactFullPath` (by @MangelMaxime)

### Changed

* [All] Invoke MSBuild from Fable current working directory (by @cr3wdayt5p)
* [Python] fable-library for Python is now distributed via PyPI instead of being bundled in the NuGet package (by @dbrattli)

## 5.0.0-alpha.15 - 2025-11-19

### Added

* [TS] Added support for `erasableSyntaxOnly` in TypeScript (by @ncave)
* [All] Added some default `System` exception implementations (by @ncave)
* [All] Added `ofOption`/`toOption`/`ofValueOption`/`toValueOption` (by @ncave)
* [Python] Added `Decorate` attribute to add Python decorators to classes (by @dbrattli)
* [Python] Added `ClassAttributes` attribute to control Python class generation (@dbrattli)
* [Python] Added support for Python 3.14 (by @dbrattli)

### Changed

* [JS/TS] Replace the deprecated `substr` method with `slice` (by @Thorium)

### Fixed

* [TS] Fix ObjectExpr tests with multiple interfaces (by @ncave)
* [TS] Fix ObjectExpr abstract base constructor type (by @ncave)
* [JS/TS] Fix #4240 Missing DateTime constructor (by @ncave)
* [PHP] Fix php import extensions (by @MangelMaxime)
* [TS] Fix #3973 Typescript imports file extension (by @ncave)
* [TS] Fix support for abstract classes and members (by @ncave)
* [TS] Fix getters, setters, indexers in interfaces (by @ncave)
* [TS] Enable typedArrays flag usage for TypeScript (by @ncave)
* [All] Fix #4221 Fable.Type union case arg names (by @ncave)
* [All] Erase Nullable Reference Types to inner type (by @ncave)
* [Python] Improve Python (e.g. Pydantic) interop (by @dbrattli)
* [JS/TS] Fix comparers for non-nullable types (by @ncave)
* [Rust] Fix curried object expression getters (by @ncave)
* [Rust] Fix compiler-generated generic args (by @ncave)
* [All] Fixed #4041 missing unit argument (by @ncave)
* [JS/TS/Python] Fixed eq comparer mangling (by @ncave)
* [All] Fix all `BitConverter` return types (by @ncave)
* [TS] Don't cast union case types to `any` (by @ncave)
* [JS/TS] Fix Dictionary.ICollection.Remove (by @ncave)
* [TS/Dart] Fixed optional parameter types (by @ncave)
* [TS] Initialize un-initialized variables (by @ncave)
* [JS/TS] Replace `Error` with `Exception` (by @ncave)
* [All] Fix MSBuildResolver to include define when restoring the project (by @MangelMaxime)

## 5.0.0-alpha.14 - 2025-07-25

### Added

* [Python] Support for Nullable Reference Types (by @dbrattli)
* [JS/TS] Fix #3533: Add directives prologues supports (by @MangelMaxime)
* [JS/TS] Support for Nullable Reference Types (by @ncave and @MangelMaxime)
* [Rust] Initial support for Nullable Reference Types (by @ncave)

### Changed

* [Python] Use Python 3.12 type parameter syntax. Deprecate Python 3.10 and 3.11 (by @dbrattli)
* [Python] Records now snake-cases all member fields (by @dbrattli)
* [Python] Anonymous records now preserves the casing of member fields (by @dbrattli)
* [Python] Option type is now `Option[T]` instead of `T | None` (by @dbrattli)
* [Python] Use [uv](https://docs.astral.sh/uv/) instead of Poetry for package management (by @dbrattli)
* [Python] Fable Library for Python is now partially written in Rust (by @dbrattli)
* [All] Improve filewatcher error messages (by @goswinr)

### Fixed

* [Python] Fixed static properties and are now translated as Python class attributes (by @dbrattli)
* [Python] Fixed DateTime with DateTimeKind generates proper enum reference (#3689) (by @dbrattli)
* [Python] Fixed Dictionary KeyValuePair enumeration when casting to IEnumerable (#3771) (by @dbrattli)
* [Python] Fixed `createEmpty<T>` for interfaces using `SimpleNamespace` with type casting (#3604) (by @dbrattli)
* [Python] Fixed EmitMethod + ParamObject losing keyword arguments (#3871) (by @dbrattli)
* [Python] Fixed EmitConstructor + ParamObject losing keyword arguments (#3871) (by @dbrattli)
* [Python] Fixed DateTimeOffset.TryParse, ToString() and Offset property access (#3854) (by @dbrattli)
* [Python] Fixed anonymous records in Maps causing comparison errors (#3869) (by @dbrattli)
* [Python] Fixed handling of erased types for Python (#3968) (by @dbrattli)
* [Python] Fixed unit function (zero arguments functions) are transpiled inconsistently (#4126) (by @dbrattli)
* [Python] Fixed resource managers with empty body (#3912) (by @dbrattli)
* [Python] Fixed `Async.Sleep`to handle TimeSpan correctly (#4137) (by @dbrattli)
* [Python] Make sure snake-cased Record do not conflict (by @dbrattli)
* [Python] Do not return None | None for optional unit types (#4127) (by @dbrattli)
* [JS/TS] JSX : Alias `empty` CEs list to `null` when encountered in the `children` list (by @MangelMaxime)
* [JS/TS] JSX : Allow usage of `unbox` when definining properties for `JSX.create` (by @MangelMaxime)

## 5.0.0-alpha.13 - 2025-05-04

### Fixed

* [Python] Fixed testing with interfaces leads to incorrect pattern match (#3972) (by @dbrattli)
* [Python] Fixed error when type contains multiple generic type parameters (#3986) (by @dbrattli)
* [Python] Fixed import path handling for libraries (#4088) (by @dbrattli)
* [Python] Reenable type aliasing for imports with name "*" (by @freymauer)
* [JS/TS] Optimise JSX output in order to avoid F# list CEs to surface in it (by @MangelMaxime)

### Removed

* [All] Remove unused `Fable.Core` copy types from internal module (by @MangelMaxime)

## 5.0.0-alpha.12 - 2025-03-14

### Added

* [Python] Add support for `nullArgCheck`(by @MangelMaxime)
* [All] Add support for F# `nullness` (by @MangelMaxime)
* [JS/TS] Add support for `Unchecked.nonNull` (by @MangelMaxime)
* [All] Add support for `TreatWarningsAsErrors` (by @MangelMaxime)
* [JS] Don't generate an import statement for pojos defined in another file (by @shayanhabibi)

### Fixed

* [JS/TS] Make `nullArgCheck` report the same error message as on .NET (by @MangelMaxime)
* [TS] Sanitize DUs case names when generating constructor function (by @MangelMaxime)

## 5.0.0-alpha.11 - 2025-03-03

### Added

* [JS/TS] Add support for `CaseRules.LowerAll` on `StringEnums` (by @shayanhabibi)
* [Rust] Support Rust 2024 language edition (by @ncave)
* [JS/TS] Add `C` and `c` format for numeric types (by @MangelMaxime)
* [JS/TS] Add `B` and `b` format for numeric types (by @MangelMaxime)
* [JS/TS] Add `n` format for numeric types (by @MangelMaxime)
* [JS/TS] Generate compiler error when detecting an invalid/unsupported format specifier for numeric types (by @MangelMaxime)

### Fixed

* [JS/TS] Fix #4025: No reflection info for pojos (by @alfonsogarciacaro)
* [JS/TS] Fix #4049: decimal/bigint to integer conversion checks (by @ncave)
* [JS/TS] Fix `decimal` to `char` conversion checks (by @ManngelMaxime)
* [JS/TS] Propagate non-captured exception when running `Async.Start` or `Async.StartImmediate` (by @MangelMaxime)
* [JS/TS] Report an error at compilation time when trying to use `Async.RunSynchronously` (by @MangelMaxime)
* [JS/TS] Fix short `DateTime` and `DateTimeOffset` short format strings (by @MangelMaxime)
* [All] Don't scan system packages for plugins (by @MangelMaxime)
* [JS/TS] Fix date formatting when repeating a format token more than the known format (example repeating 'd' more than 4 times) (by @MangelMaxime)
* [Python] Fix date formatting when repeating a format token more than the known format (example repeating 'd' more than 4 times) (by @MangelMaxime)
* [JS/TS] Fix #4010: Supports direct nested types when using `jsOptions` (by @MangelMaxime)

    ```fs
    let opts =
        jsOptions<Level1> (fun o ->
            o.level2.level3.valueA <- 10
            o.level2.level3.valueB <- 20
            o.topValueA <- 20
        )
    ```

* [JS/TS] Fix numeric formats (by @MangelMaxime)

### Changed

* [JS/TS] Throw an error is an invalid Numeric format is provided (mimic .NET behavior) (by @MangelMaxime)

## 5.0.0-alpha.10 - 2025-02-16

### Added

* [Python] - Print root module and module function comments (by @alfonsogarciacaro)
* [Rust] Add support for module comments (by @ncave)
* [Rust] Add support for null strings (by @ncave)
* [TS/JS] `Pojo` attribute support (by @alfonsogarciacaro)

### Fixed

* [JS/TS] - Fix anonymous record printing (#4029) (by @alfonsogarciacaro)
* [Python] - Fix #3998: PhysicalEquality (by @alfonsogarciacaro)
* [Python] Resolve relative paths for non-qualified imports (#3481) (by @alfonsogarciacaro)
* [Python] `importSideEffects` shouldn't generate identifier (#3965) (by @alfonsogarciacaro)
* [JS/TS] Fix #4031: Hoist vars locally in for and while loops (@alfonsogarciacaro)

### Changed

* [JS/TS] In `JSX`, generate self closing element when element has no children (#4037) (by @shayanhabibi)

## 5.0.0-alpha.9 - 2025-01-28

### Fixed

* [All] Add `--realsig+` to fix `Method not found: 'Boolean Fable.CompilerOptions.Equals` (by @nojaf)

## 5.0.0-alpha.8 - 2025-01-27

### Added

* [TS] Include XML Doc comment on interface properties (by @Freymaurer)
* [TS] Generate `interface` type when using the "ParamObject" class pattern (by @MangelMaxime)
* [Rust] - Initial support for null values (by @ncave)

## 5.0.0-alpha.7 - 2025-01-23

### Fixed

* [All] Disable .NET Welcome message when cracking project (#4014) (by @MangelMaxime)

## 5.0.0-alpha.6 - 2025-01-23

### Added

* [All] Add `StringBuiler.Append(c: char, repeatCount: int)` overload (by @roboz0r)
* [All] Added primitive types equality and comparison (by @ncave)
* [All] Updated FCS to latest F# 9.0 (by @ncave)
* [All] Updated Fable-FCS to latest F# 9.0 (by @ncave)
* [All] Updated metadata to latest .NET 9.0 (by @ncave)
* [All] Updated FCS type constraints (by @ncave)

### Fixed

* [Python] Add missing unicode categories in python library (by @joprice)
* [All] Log JSON output if we fail to parse MSBuild result (by @MangelMaxime)

## 5.0.0-alpha.5 - 2025-01-09

### Added

* [JS/TS/Python] Add new `TimeSpan` overloads support coming from .NET 9.0 (by @MangelMaxime)
* [Rust] Add new `TimeSpan` overloads support coming from .NET 9.0 (by @ncave)

### Fixed

* [JS/TS] Fix `DateTimeOffset.ToLocalTime` (by @MangelMaxime)

## 5.0.0-alpha.4 - 2025-01-03

### Fixed

* [TS] Make discriminated union `.Is*` properties works (@MangelMaxime)
* [JS/TS/Python] Fix `h` in `DateTime.ToString` (@MangelMaxime)
* [JS/TS] Fix `hh` in `DateTime.ToString` (@MangelMaxime)
* [JS/TS] Don't generate the setter code if a property is decorated with `[<Erase>]` (@MangelMaxime)

## 5.0.0-alpha.3 - 2024-12-18

### Fixed

* [Python] Fix type testing against `uint8`, `uint32`, `uint64`, `decimal` (@MangelMaxime)
* [JS/TS] Workaround source map generation bug (deteriorate them a little) (@MangelMaxime)

## 5.0.0-alpha.2 - 2024-11-25

### Fixed

* [All] Allow Fable 5 to be used with Fable 4 plugins (@ncave)

## 5.0.0-alpha.1 - 2024-11-24

### Added

* [All] Add `--legacyCracker` as a fallback (@MangelMaxime)

### Changed

* [All] Make MSBuildCracker the default (@MangelMaxime)
* [All] Replace `FABLE_COMPILER_4` with `FABLE_COMPILER_5` as the compiler directive (@MangelMaxime)
* [All] Move TargetFramework to `net8.0` (@ncave)

## 4.0.0-alpha-016 - 2024-11-19

### Changed

* Fable 4.24.0

## 4.0.0-alpha-015 - 2024-10-28

### Changed

* Fable 4.23.0

## 4.0.0-alpha-014 - 2024-10-02

### Changed

* Fable 4.22.0

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
