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
