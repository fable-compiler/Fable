# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## 4.2.0 - 2021-10-24

### Added

#### JavaScript

* Add `NumberConstructor.EPSILON` (by @MangelMaxime)
* Add `NumberConstructor.isFinite` (by @MangelMaxime)
* Add `NumberConstructor.isInteger` (by @MangelMaxime)
* Add `NumberConstructor.isSafeInteger` (by @MangelMaxime)
* Add `NumberConstructor.MAX_SAFE_INTEGER` (by @MangelMaxime)
* Add `NumberConstructor.MIN_SAFE_INTEGER` (by @MangelMaxime)
* Add `NumberConstructor.parseFloat` (by @MangelMaxime)
* Add `NumberConstructor.parseInt` (by @MangelMaxime)

### Changed

#### JavaScript

* Change `NumberConstructor.isNaN` from `float -> bool` to `obj -> bool` (by @MangelMaxime)

## 4.1.1 - 2023-10-18

### Added

#### Python

* Fix #3482: Revert removal of `Py.python` and `Py.expr_python` (by @dbrattli)

## 4.1.0 - 2023-09-29

* Fix #3482: Remove `Py.python` and `Py.expr_python`
* Add `!^` to `Fable.Core.RustInterop` module
* Fix #3484: Rename `emitStatement` to `emitPyStatement` in `PyInterop`
* Fix #3484: Rename `emitExpr` to `emitPyExpr` in `PyInterop`
* Fix #3484: Replace `Rust.emitExpr` with `RustInterop.emitRustExpr`

## 4.0.0-beta-001 - 2022-09-07

* Fable 4 stable

## 4.0.0-theta-007 - 2022-11-13

* Fix #3249 by enabling uncurrying for functions with multiple parameters @inosik

## 4.0.0-theta-006 - 2022-10-14

* Python fixes for decorator attribute

## 4.0.0-theta-005 - 2022-10-04

* Add JSX.jsx function

## 4.0.0-theta-004 - 2022-09-28

* Add ??= operator to JsInterop

## 4.0.0-theta-003 - 2022-09-22

* Python fixes for Interactive

## 4.0.0-theta-002 - 2022-09-21

* Python helpers for Interactive
* JS helpers for JSX/React integration

## 4.0.0-theta-001 - 2022-09-09

* Fable 4 theta release

## 4.0.0-snake-island-alpha-007 - 2022-07-25

* Snake Island alpha release

## 4.0.0-snake-island-alpha-006 - 2022-07-12

* Some Rust helpers

## 4.0.0-snake-island-alpha-005 - 2022-06-03

* DartNullable and other Dart helpers

## 4.0.0-snake-island-alpha-004 - 2022-05-25

* JSX string templates

## 4.0.0-snake-island-alpha-003 - 2022-05-23

* Add Dart print/Future/Stream

## 4.0.0-snake-island-alpha-002 - 2022-05-20

* JSX Support

## 4.0.0-snake-island-alpha-001 - 2022-05-11

* Snake Island alpha release

## 3.7.1 - 2022-05-27

* Add `AsyncIterable` to Fable.Core.JS

## 3.7.0 - 2022-04-29

* Fix #2863: Add a nestable union type U9 @cannorin
* Fix links in doc comments

## 3.6.2 - 2022-02-17

* Fix #2805: Improve jsNative error messages
* Revert changes in Core.JS, see Fulma/issues/294

## 3.6.1 - 2021-11-26

* TypescriptTaggedUnion @cannorin

## 3.6.0-beta-001 - 2021-11-17

* Add Fable.Core.Compiler.triggeredByDependency flag

## 3.4.0 - 2021-10-01

* ParamObject attribute

## 3.3.1 - 2021-09-16

* JS.Decorator/ReflectedDecorator attributes
* JS.Function

## 3.2.9 - 2021-08-26

* Add `FormattableString.GetStrings()` extension

## 3.2.8 - 2021-06-11

* Experimental.namesofLambda

## 3.2.7 - 2021-05-14

* Publish with icon and symbols @cartermp

## 3.2.6 - 2021-04-09

* Experimental `casenameWithFieldIndex`

## 3.2.5 - 2021-03-11

* Add `Compiler.extension`

## 3.2.4 - 2021-02-12

* Add Object.values, Object.entries @kerams
* Add JsInterop.jsTypeof/jsInstanceof
* Add JS.Constructors.Array.Create/from

## 3.2.3 - 2021-01-08

* Fix incompatibility with Feliz @TIHan

## 3.2.2 - 2020-12-22

* `AttachMembers` attribute

## 3.2.1 - 2020-12-16

* Lower FSharp.Core requirement to 4.7.2

## 3.2.0 - 2020-12-04

* New helpers for Fable 3
* `Mangle` attribute
* `isStatement` parameter for `Emit` attribute
* `emitJsExpr` and `emitJsStatement` helpers
* Access compiler options in `Compiler` module

## 3.1.6 - 2020-09-22

* Fixed TypedArray.set signature @Shmew

## 3.1.5 - 2020-02-10

* Fixed missing indexer in JS TypedArray.

## 3.1.4 - 2019-12-23

* Fix JS Map constructor @Luiz-Monad

## 3.1.3 - 2019-11-07

* PR #1935: Put JS constructors in different module to prevent conflicts @pauldorehill

## 3.1.2 - 2019-10-14

* Add CaseRules.SnakeCase

## 3.1.1 - 2019-09-25

* Fix TypedArray bindings @Titaye

## 3.1.0 - 2019-09-17

* Add JS Typed Arrays @Titaye
* Fix names of arguments in Testing.Assert @zanaptak

## 3.0.0-beta-001 - 2019-02-28

* Add `JsInterop.importValueDynamic`
* Move `nameof` operators to Experimental module
* JS extensions
* Add Emit and Import attribute aliases
* Namespace restructure

## 3.0.0-beta-006 - 2019-04-12

* Add `JsInterop.importValueDynamic`

## 3.0.0-beta-005 - 2019-03-13

* Move `nameof` operators to Experimental module

## 3.0.0-beta-004 - 2019-03-12

* Move JS extensions to Fable.Core.Extensions file

## 3.0.0-beta-003 - 2019-03-05

* Add some missing JS APIs

## 3.0.0-beta-002 - 2019-03-04

* Array .buffer extension
* Add Emit and Import attribute aliases

## 3.0.0-beta-001 - 2019-02-28

* Namespace restructure

## 2.1.0-alpha-002 - 2019-01-22

* Clean Fable.Import.JS module and rename it to Fable.Core.JS
* Add unions of measure for non-numeric primitive types

## 2.0.3 - 2018-12-10

* Add `Fable.Core.Reflection` module

## 2.0.2 - 2018-11-20

* Add `nameof2` and `exportDefault`

## 2.0.1 - 2018-10-26

* Fix `ReadonlyArray` spelling

## 2.0.0-alpha-001 - 2018-05-24

* Fable 2

## 1.3.7 - 2017-12-21

* See dotnet-fable 1.3.7 release notes

## 1.3.0-beta-009 - 2017-11-15

* See dotnet-fable 1.3.0-beta-009 release notes

## 1.2.4 - 2017-09-27

* See dotnet-fable 1.2.4 release notes

## 1.2.3 - 2017-09-18

* Fix #1135, #1136: `unit option` hack doesn't work with generic functions

## 1.2.2 - 2017-09-18

* See Fable.Compiler 1.2.2

## 1.2.1 - 2017-09-10

* Guid validation and byte[] conversion
* Date Fix #1139

## 1.2.0-beta-001 - 2017-08-23

* Add support for String.filter #1133
* Fix #1131: DateTime.ToString not working without separator

## 1.2.0-beta-004 - 2017-09-05

* Fix format over local dates #1129
* Fix 2 digits years datetime #1127

## 1.2.0-beta-003 - 2017-08-31

* Don't calculate relative path for paths in different drive letters on Windows

## 1.2.0-beta-002 - 2017-08-30

* Target netstandard1.6 again

## 1.2.0-beta-001 - 2017-08-23

* Upgrade to netstandard2.0

## 1.1.15 - 2017-08-22

* Fix #1113: Set.union

## 1.1.14 - 2017-08-08

* Fix #1104: JSON serialization of Long and BigInt

## 1.1.12 - 2017-07-31

* Add DateTime ticks constructor
* Convert to and from Base64String

## 1.1.11 - 2017-07-27

* Fix #1094: Implement "indexed" in collection modules

## 1.1.10 - 2017-07-24

* Fix recursive functions in Seq module

## 1.1.9 - 2017-07-22

* Add constructors to JsFunc

## 1.1.8 - 2017-07-18

* Small refactoring
* Add `DateTime.IsDaylightSavingTime` replacement (#1082, #1083)

## 1.1.7 - 2017-07-11

* Support String.Join with objects (#1058)

## 1.1.6 - 2017-07-04

* Fix #1046: Parse time-only strings
* Support some System.Uri static methods (#1048)

## 1.1.5 - 2017-06-28

* Fix #1028: Arguments of auto-generated lambdas conflicting with outer variables

## 1.1.4 - 2017-06-24

* Fix fable-import/#9: Don't try to replace method calls in bindings

## 1.1.3 - 2017-06-20

* Add System.Console reference

## 1.1.2 - 2017-06-20

* Fix Paket groups for Fable.Core

## 1.1.1 - 2017-06-19

* Fix F# compiler errors in recompilation
* Fix Elmish.Browser parser (#1003)
* Add support for Option.defaultValue
* Warn when there are nested options
* Add replacement for System.Environment.NewLine (#993)
* Expose Console among JS import globals

## 1.1.0-rc-001 - 2017-06-10

* Fable stablish, yeah!

## 1.1.0-rc-002 - 2017-06-12

* Fix #996: Don't wrap dynamic CurriedLambdas

## 1.1.0-rc-001 - 2017-06-10

* Support Paket groups and move Browser and Node bindings out of Fable.Core

## 1.0.8 - 2017-06-02

* Fix #940: Compilation of multiple projects in parallel

## 1.0.7 - 2017-05-27

* Fix #952: Don't remove non-null unit arguments
* Fix #944: Import and Emit on same function

## 1.0.6 - 2017-05-24

* Propagate subprocess exit code

## 1.0.5 - 2017-05-24

* Fix #946: --port free throws exception

## 1.0.4 - 2017-05-17

* Don't redirect dotnet-fable commands but check version and locate fable-core JS files in packages/Fable.Core

## 1.0.0-narumi-917 - 2017-05-16

* Fix location of fable-core files

## 1.0.0-narumi-916 - 2017-05-15

* We're going PAKET!

## 1.0.0-narumi-915 - 2017-05-09

* Fix curried lambdas assigned to generic params (see #888)

## 0.7.49 - 2017-03-29

* Fix #744: CLI argument flags

## 0.7.48 - 2017-03-29

* Fix #736: Case testing with erased unions

## 0.7.47 - 2017-03-29

* Tailcall optimizes function arguments: #681

## 0.7.45 - 2017-03-29

* Fix import expressions with methods with function arguments: #721

## 0.7.43 - 2017-03-29

* Add reflection methods (see ReflectionTests.fs)
* Improve import expressions #721

## 0.7.42 - 2017-03-29

* Tailcall optimizations: PR #669 + Fixes

## 0.7.37 - 2017-03-29

* Add FSharp.Core.dll to fable-compiler package
* Improve handling of `jsThis`

## 0.7.35 - 2017-03-29

* Only show Rollup warnings in `--verbose` mode
* Minor fixes

## 0.7.34 - 2017-03-29

* Fix #660: `Option.fold` and `Option.foldBack`

## 0.7.33 - 2017-03-29

* Add operator `enum`

## 0.7.32 - 2017-03-29

* Fixed default comparer: PR #658

## 0.7.31 - 2017-03-29

* Added BigInt conversions: PR #650

## 0.7.30 - 2017-03-28

* Fix #649: Int64 serialization
* Fix #648: Caching of namespace F# entities
* Fix #646: Zero fill shift right (>>>) for uint32

## 0.7.29 - 2017-03-14

* Add `BigInteger` support

## 0.7.28 - 2017-02-21

* Fix #640: Omitted non optional nulls
* Fix #638: Inlined methods with `this` argument

## 0.7.27 - 2017-02-21

* Add `--includeJs` compiler argument
* Fix #629: Visual Studio error format
* Fix  #637: IEnumerable runtime issue

## 0.7.26 - 2017-01-25

* Fixed Option.foldBack (#634)
* Improve .NET Enumerator/JS Iterator compatibility
* Fix #633: jsThis
* Fixed Option.foldBack (PR #634)

## 0.7.25 - 2017-01-20

* Fix #623: JS Global properties
* Fix #624: Aether compilation

## 0.7.24 - 2017-01-19

* Fix module generic methods without arguments
* Added `String.IndexOfAny/Compare/Equals` with `StringComparison` enum
* Fix generic types when compiling to ES2015
* Unify `Equals` and `CompareTo` methods of `Long.ts`

## 0.7.23 - 2017-01-17

* Fix #348 #608 Double/Int32 Parse & TryParse
* Fix #609: Event unsubscription
* Fix custom exception type testing in ES5
* Inline local lambdas as values (#601)
* Fix edge case in `toJson` (#611)
* Fix fable-core Babel compilation
* Added 64-bit integers (#600)
* Fix #237: Recursive value definitions
* Added char to int (#594)

## 0.7.22 - 2017-01-13

* Fix secondary constructors of imported classes
* Improvements in option handling

## 0.7.20 - 2017-01-09

* Prevent infinite loops for bundle errors in watch mode

## 0.7.19 - 2017-01-06

* Fix #509: Add KeyValuePattern
* Fix #544 #585: Bundling in watch mode
* Fix #589: Regex Provider with Fable 0.7
* Add optional argument to GlobalAttribute

## 0.7.18 - 2017-01-05

* Add validation to `ofJson`
* Make `Assert.AreEqual` work with F# equality
* Improve dynamic programming (add `jsThis`)
* Add `Printf.kprintf` (#583)
* Fix sprintf "%X" (#580)
* Fix #579: Printf.printfn

## 0.7.17 - 2016-12-30

* Add `Pojo` union types

## 0.7.16 - 2016-12-26

* Fix issues in generic resolution

## 0.7.15 - 2016-12-06

* Issue warning when calling `typeof` on a generic parameter
* Use absolute paths for assembly references in `node_modules`

## 0.7.14 - 2016-12-04

* Use "umd" distribution of fable-core and other libs when compiling for umd modules
* Fix #569: Types with circular dependencies

## 0.7.12 - 2016-12-01

* Fix #568: Types with `StructAttribute` (compiled as records)
* Better checking when calling functions with `PassGenericsAttribute`

## 0.7.11 - 2016-11-28

* Some adjustments in attribute resolution

## 0.7.10 - 2016-11-27

* Fix #565, #566: Only watch F# files
* Fix #564: Don't overload methods from imported classes
* Fix #563: Overloads with `PassGenericsAttribute`

## 0.7.9 - 2016-11-24

* Fix `Array.rev` (see #559)
* Added UnboxFast intrinsic / List.truncate (see #561)

## 0.7.8 - 2016-11-22

* `System.Exception` translates to JS `Error`
* Fix #556: inline methods placed after calls

## 0.7.7 - 2016-11-22

* Fix ES2015 imports

## 0.7.6 - 2016-11-22

* Too many changes to be listed, check the [migration guide](http://fable.io/blog/Introducing-0-7.html)

## 0.7.5-alpha.41 - 2016-11-22

* Fix cyclic dependencies in fable-core

## 0.7.5-alpha.40 - 2016-11-22

* Minor fix

## 0.7.5-alpha.39 - 2016-11-22

* Simplify Reflection system
* Add `PojoAttribute`
* Replace baseDir of references with --refs argument

## 0.7.5-alpha.38 - 2016-11-22

* Fix #547: Ignored entities conflicting when calculating root namespace

## 0.7.5-alpha.37 - 2016-11-22

* Mangle class methods when they conflict with an implemented interface

## 0.7.5-alpha.35 - 2016-11-22

* Fix #545: Using `Microsoft.FSharp` namespace

## 0.7.5-alpha.34 - 2016-11-22

* Add `import: selector->path->'T` to import expressions
* Make arguments of JsConstructor statically typed
* Add logs when compiling files for bundling

## 0.7.5-alpha.33 - 2016-11-22

* Fix default watch directories
* Ignore interfaces

## 0.7.5-alpha.32 - 2016-11-22

* Compile scripts referenced as plugins
* Apply Replace plugins to any call
* Include in compilation JS files with relative paths

## 0.7.5-alpha.31 - 2016-11-22

* Fix #535: Script referenced entity replacements

## 0.7.5-alpha.30 - 2016-11-22

* Allow configuration of watching dirs

## 0.7.5-alpha.29 - 2016-11-22

* Fix infinite recursion when resolving generic parameters

## 0.7.5-alpha.28 - 2016-11-22

* More meaningful runtime representation of non-declared types

## 0.7.5-alpha.27 - 2016-11-22

* When inlining, assign arguments referenced more than once to a temp var
* `Array.zeroCreate` fills non-numeric arrays with null

## 0.7.5-alpha.26 - 2016-11-22

* Resolve trait calls as normal method calls (check EmitAttribute, etc)

## 0.7.5-alpha.24 - 2016-11-22

* Fix mangled interfaces
* Improve error messages

## 0.7.5-alpha.23 - 2016-11-22

* Add `MangleAttribute` to prevent conflicts with interfaces
* Allow combination of `ImportAttribute` and `EmitAttribute`
* Several fixes

## 0.7.5-alpha.22 - 2016-11-22

* Add catch-all for missing replacements when referencing
  types of the BCL or FSharp.Core
* Omit `.js` extension again in imports to keep
  compatibility with Require.js

## 0.7.5-alpha.21 - 2016-11-22

* Don't print warnings in process.stderr (see #516)
* Add `String/formatError` (see #519)
* Add `.js` extension to `fable-core` and internal imports

## 0.7.5-alpha.19 - 2016-11-22

* Distribute fable-core with ES2015 (default) and UMD module formats

## 0.7.5-alpha.18 - 2016-11-20

* Don't deem interface setters as overloads (see #505)

## 0.7.5-alpha.17 - 2016-11-16

* Update FCS and use Forge to read .fsproj files (removes MSBuild dependency)
* Bug fixes and optimizations

## 0.7.5-alpha.16 - 2016-11-13

* Add JsFunc and JsCons to Fable.Core.JsInterop

## 0.7.5-alpha.15 - 2016-11-12

* Use outDir to place the generated dll with .fsproj projects
* Only emit warnings when generating dlls in verbose mode
* Fix error when reading Rollup options

## 0.7.5-alpha.14 - 2016-11-09

* Fix errors in Fable JS API

## 0.7.5-alpha.12 - 2016-11-04

* Change `--bundle` option to `--rollup`
* `--rollup` can accept an object (in fableconfig.json) with Rollup config
* Improve plugin resolution for Babel and Rollup

## 0.7.5-alpha.11 - 2016-10-30

* Add a block scope to switch cases (see #483)

## 0.7.5-alpha.10 - 2016-10-29

* Fix invalid identifiers in top level members (see #482)
* Generate `dll` also for `fsproj` files
* Expose Fable library to JS API

## 0.7.5-alpha.9 - 2016-10-29

* Fix issues with bundling and `System.Exception`

## 0.7.5-alpha.8 - 2016-10-28

* Fix problems with `dll` generation

## 0.7.5-alpha.7 - 2016-10-23

* Add experimental `--dll` compiler option to generate assemblies

## 0.7.5-alpha.6 - 2016-10-22

* Fix path resolution when referencing assemblies

## 0.7.5-alpha.5 - 2016-10-19

* Fix partial patterns not returning any value (see #478)

## 0.7.5-alpha.4 - 2016-10-19

* Minor fixes

## 0.7.5-alpha.2 - 2016-10-17

* Resolve relative paths of referenced projects/dlls as
  if they were pointing to generated JS code from the fsproj/dll file
  (`EntryModuleAttribute`) or from working directory (`--refs`)
  and make them reachable from `outDir` (see #472)
* Always resolve relative paths in command line options with
  the directory from where fable is called

## 0.7.5-alpha.1 - 2016-10-16

* Fix prerelease semver. [See documentation](https://docs.npmjs.com/misc/semver#prerelease-tags)

## 0.7.5-alpha - 2016-10-16

* Add `typedefof<>`, `.IsGenericType`, `.GetGenericTypeDefinition`

## 0.7.4-alpha - 2016-10-15

* Resolve import relative paths so they can be reached from `outDir`
  if they don't point to an internal file (see #472)

## 0.7.3-alpha - 2016-10-15

* Add warning when creating references of types unknown at compile time

## 0.7.2-alpha - 2016-10-14

* Convert functions to delegates when passed as arguments to EmitAttribute
* Add warning when passing generic param to Serialize.ofJson/inflate

## 0.7.1-alpha - 2016-10-13

* Add `--bundle` compiler option
* Bring back JSON serialization with `$type` info

## 0.7.0-alpha - 2016-10-10

* Add type info to JS constructors: cases (unions) and properties (records and classes)
* Extend type references with generic info when calling `typeof`
* Add `GenericParamAttribute` to implicitly pass type info of generic parameters
* Add `MutatingUpdateAttribute` to compile record updates as mutations
* Add `EntryModuleAttribute` to mark assemblies as Fable libraries

## 0.6.15 - 2016-10-11

* Revert change from 0.6.14: ES6 modules automatically enable
  strict mode, so the directive is not needed

## 0.6.14 - 2016-10-11

* Add always "use strict" directive

## 0.6.12 - 2016-10-11

* Now fableconfig.json can be in JSON5 format (comments!)
* Omit lambda argument when it's of unit type

## 0.6.11 - 2016-10-11

* Don't wait until "postbuild-once" script is finished (see #432)

## 0.6.9 - 2016-10-11

* Fix #431: Add --babelrc compiler option
* Fix #432: Add "postbuild-once" script
* Fix #433: Add --noTypedArrays compiler option
* Fix #436: Inform users about not supported overloads of String.IndexOf/LastIndexOf
* Fix #438: Incorrect this in local let functions creating object expressions
* Fix #439: IDE locking project file and preventing watch compilations
* Change compilation defaults: module=commonjs & loose=true

## 0.6.7 - 2016-10-02

* Fix #416: Can't ifdef around a load directive
* Fix List.unfold: PR #428
* Use reference hint paths for netcore: PR #423

## 0.6.6 - 2016-09-30

* Support ResizeArray.FindAll: PR #412

## 0.6.5 - 2016-09-29

* Support decimal (converted to JS number): PR #413
* Support recursive records: PR #417

## 0.6.4 - 2016-09-28

* Fix #411: for .. downto

## 0.6.3 - 2016-09-15

* Fix watch compilation on Windows

## 0.6.2 - 2016-09-15

* Integer conversions (see #407)

## 0.6.1 - 2016-09-12

* Multiple bug fixes
* Internal Fable AST additions
* [Rewrite plugins](https://fable-compiler.github.io/docs/plugins.html#Creating-rewriter-plugins)
* Use let/const when compiling value bindings to ES6
* Compile pattern matching with string or number literals as switch statements

## 0.5.11 - 2016-09-11

* Fix #382 (partially): Exclude files in "node_modules" when calculating the base
  directory of referenced dlls.

## 0.5.10 - 2016-09-11

* Fix #369: Extra blank lines when running postbuild script
* Fix #375: Add warning when replacements change semantics (printf, Async.Start...)
* Fix #377: Remove null args at the end of method/constructor calls also with macros

## 0.5.9 - 2016-09-11

* Fix type testing with Erased Unions
* Erase assigments generated for tuple, record and union gets
* Fix #370: Add warning when using unions or records with Dictionary (as keys) or HashSet
* Internal cleanup

## 0.5.8 - 2016-09-11

* Use chokidar to watch for file changes
* Fix calculation of relative paths with special characters (like "#")
