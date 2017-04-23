### 1.0.0-narumi-912

* Test

### 0.7.28

* Fix exception propagation with Async.Bind: #724

### 0.7.27

* Add reflection methods (see ReflectionTests.fs)

### 0.7.26

* Fix #666: F# record equality and comparison checks only declared members

### 0.7.25

* Fix #660: `Option.fold` and `Option.foldBack`

### 0.7.24

* Fixed serialization of maps with string keys: #659

### 0.7.23

* Added BigInt conversions: PR #650

### 0.7.22

* See fable-compiler 0.7.30 release notes

### 0.7.21

* Add `BigInteger` support

### 0.7.20

* Add `IDictionary` interface to F# Map

### 0.7.19

* See fable-compiler 0.7.26 release notes

### 0.7.18

* Allow inflation of lists serialized with `JSON.stringify`

### 0.7.17

* See fable-compiler 0.7.24 release notes

### 0.7.16

* See fable-compiler 0.7.23 release notes

### 0.7.15

* Add optional argument to GlobalAttribute

### 0.7.14

* Add validation to `ofJson`
* Make `Assert.AreEqual` work with F# equality
* Improve dynamic programming (add `jsThis`)
* Add `Printf.kprintf` (#583)
* Fix sprintf "%X" (#580)
* Fix #579: Printf.printfn

### 0.7.12

* Add helpers for dynamic programming

### 0.7.11

* Fix #569: Types with circular dependencies

### 0.7.10

* Fix reflection with nested options

### 0.7.9

* Fix #560: `typeof<'T>.FullName`

### 0.7.8

* Fix #557: `Array.distinctBy`

### 0.7.7

* `System.Exception` translates to JS `Error`

### 0.7.6

* Too many changes to be listed, check the [migration guide](http://fable.io/blog/Introducing-0-7.html)

### 0.7.5-alpha.18

* Fix cyclic dependencies

### 0.7.5-alpha.18

* Simplify Reflection system

### 0.7.5-alpha.17

* Remove `NoMangle` attribute

### 0.7.5-alpha.16

* Add `import: selector->path->'T`  to import expressions
* Make arguments of JsConstructor statically typed

### 0.7.5-alpha.15

* Add extra argument to `EmitAttribute`

### 0.7.5-alpha.14

* More meaningful runtime representation of non-declared types

### 0.7.5-alpha.12

* Add `MangleAttribute` to prevent conflicts with interfaces

### 0.7.5-alpha.11

* Omit `.js` extension again to keep compatibility with Require.js

### 0.7.5-alpha.10

* Add `.js` extension to internal `fable-core` imports
* Add `String/formatError` (see #519)

### 0.7.5-alpha.9

* Distribute fable-core with ES2015 (default) and UMD module formats

### 0.7.5-alpha.8

* Bug fixes and optimizations

### 0.7.5-alpha.7

* Add JsFunc and JsCons to Fable.Core.JsInterop

### 0.7.5-alpha.6

* Add DateTime.TryParse

### 0.7.5-alpha.5

* Add .d.ts declaration files

### 0.7.5-alpha.4

* Add String.Insert method

### 0.7.5-alpha.3

* Accept any object as exception

### 0.7.5-alpha.2

* Structure fable-core in modules

### 0.7.5-alpha.1

* Fix prerelease semver. See: https://docs.npmjs.com/misc/semver#prerelease-tags

### 0.7.5-alpha

* Add `typedefof<>`, `.IsGenericType`, `.GetGenericTypeDefinition`

### 0.7.4-alpha

* Add `outDir` to Fable.CompilerOptions (see #472)
* Make `GetType` return "boolean", "number", "string" or "function"
  for primitive values (see https://goo.gl/J6GeKY)

### 0.7.3-alpha

* Optimize JSON serialization of union types (as in FSharpLu.Json)

### 0.7.2-alpha

* Improve tree shaking

### 0.7.1-alpha

* Bring back JSON serialization with `$type` info

### 0.7.0-alpha

* Serialize.ofJson doesn't need JSON to contain `$type` info any more

### 0.6.10

* Add `String.Remove`

### 0.6.9

* Fix #466: Add password and username to Fable.Import.Browser.Location

### 0.6.8

* Fix #459: Seq.filter doesn't blow the stack with long sequences

### 0.6.7

* List.groupBy now returns valid list (see PR #453)
* Fix #454: Range sequence iterators not rewinding

### 0.6.6

* Fix #440: Order of values in Seq.groupBy

### 0.6.5

* Fix nested failures in async expressions

### 0.6.4

* Fix Seq/List/Array.pairwise

### 0.6.3

* Fix sprintf "%O" with types overloading ToString

### 0.6.2

* Fix groupBy with structural equality

### 0.6.1

* Fix Array/List/Seq.tryFindIndexBack (PR #410)

### 0.6.0

* Bug fixes
* Implement Seq.except
* Move `ofJson` and `toJson` away from `Util` to improve dead code removal

### 0.5.5

* Update README
