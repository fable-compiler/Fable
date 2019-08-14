### 2.3.19

* Fix #1876 (bis): script (fsx) compilation @nojaf
* Fix #1880: parsing uint64
* Support RegexOptions.Singleline
* Minor optimization and sourcemaps improvement

### 2.3.18

* Fix #1876: script (fsx) compilation @nojaf
* Fix struct without explicit constructor @0x53A

### 2.3.17

* Update FSharp.Compiler.Services and Dotnet.ProjInfo
* Fix #1868: Type provider ResolutionFolder is empty @ncave

### 2.3.16

* Fix #1871: comparing anonymous record types
* Fix `TimeSpan.ToString` for negative values @MangelMaxime
* Some more source map improvements

### 2.3.15

* Fix negative timespan + milliseconds value when parsing a string @MangelMaxime
* Improve source maps @matthid

### 2.3.14

* Fix Timespan and Guid reflection info @MangelMaxime
* Fix #1864: npm security issues
* Fix #1863: Binding event to a value
* Fix #1862: Add `--silent` option

* Fix #1859: Binding optimization vs tailcalls
* Fix #1857, #1729: Maps casted to IDictionary
* Fix #1856: Decimal and BigInt ranges

### 2.3.12

* Fix #1848: decimal should round up .5

### 2.3.11

* Fix #1845: Seq/Array/List.allPairs
* Fix #1844: DateTime.SpecifyKind
* Fix #1843: Remove project.assets.json check
* Fix #1842: Add Option.map2/map3

### 2.3.10

* Fix #1836: Partial Applying caches side-effects
* Bring beta reduction optimizations

### 2.3.8

* Fix #1832: unit arguments

### 2.3.7

* Fix SRTP with anonymous records

### 2.3.6

* Fix #1825: FSharp.Reflection: MakeTupleType
* Fix anonymous record casting with !!

### 2.3.5

* Raise error if two or more record/interface/override members have the same name.

### 2.3.4

* Fix #1821: decimal division @ncave
* Fix #1817: List slicing

### 2.3.3

* Reflections support for anonymous records

### 2.3.2

* Update Dotnet.ProjInfo

### 2.3.1

* Optimize `ResizeArray.Count`

### 2.3.0

* Give warnings if an anonymous record casted to an interface with `!!` mismatches any field
* Compatibility with `JsInterop.importValueDynamic`

### 2.2.3

* Fix BitConverter @ncave
* Raise error when inlining functions that reference private members

### 2.2.2

* Update FCS

### 2.2.0

* Stable version
* Make dictionary throw key not found when indexing @xdaDaveShaw

### 2.2.0-beta-019

* Compile .fsx scripts @nojaf
* Fix #1794: System.String.Format does not pad correctly in hex formatting @ncave
* Fix #1792: Decimal does not support DivideByInt @ncave
* Fix #1791: Cyclic dependencies in Array.js/List.js
* Fix #1790: Dictionary.Add throws on duplicated keys @xdaDaveShaw
* Fix #1751: ignore Babel config files for AST

### 2.2.0-beta-018

* Fix #1784: Decimal.ToString @ncave
* Fix #1785 CancellationToken.Register
* Fix #1782: Map.count
* Fix #1779: Local function imports
* Remove duplicated sources
* Lock Console.Out when writing

### 2.2.0-beta-016

* Fix #1769: Handle empty strings when parsing dates @Zaid-Ajaj
* Raise type resolve error for non-inlined functions requiring Type.Name @Zaid-Ajaj
* Emit attribute aliases

### 2.2.0-beta-015

* Improve sourcemaps

### 2.2.0-beta-014

* Add bigint xor operator, bigint byte array conversions, and Random.NextBytes @chadunit
* Fix Decimal.MinValue/MaxValue
* Fix provider argument warnings

### 2.2.0-beta-012

* Fix #1757: Don't do a `this` null check in constructors
* Fix #1753: Regex Group values can be converted to int @vbfox
* Fix #1749: Cancel Async.Sleep @SirUppyPancakes
* Fix #1745: bigint from uint32 @chadunit
* Fix #1744: BigInteger.DivRem @chadunit
* Fix #1738: List/Array.groupBy doesn't preserve order @ncave
* Update to latest FCS @ncave
* Ignore type info of provided erased types
* Support EmitDeclaration attribute

### 2.2.0-beta-010

* Anonymous records!

### 2.2.0-beta-007

* Fix report of warnings as errors from files in `.fable` dir

### 2.2.0-beta-006

* Merge with master

### 2.2.0-beta-005

* Fix warning filters through .fsproj options

### 2.2.0-beta-004

* Fix dotnet assembly name

### 2.2.0-beta-002

* Fix fable-library

### 2.2.0-beta-001

* Use ncave slim service for F# compiler

### 2.1.12

* Fix #1724: Handle nuspec files without dependency groups @inosik
* Added Decimal constructors @ncave
* Added ValueOption @ncave
* Fix #1718: Parsed guids with different case are considered the same

### 2.1.11

* Add `List.exactlyOne` @wangweipeng
* Add support for more `TimeSpan.ToString` options @rfrerebe
* Added `ResizeArray.RemoveAll/FindIndexFindLastIndex` @ncave
* Fix #1705: `Array.choose` always returns empty array for numeric option output types @ncave
* Fix #1716: `windowed` is not supported
* Fix #1715: `Seq.forall` is not lazy
* Fix #1712: HashSet equality works with generics
* Fix #1696: Add warning when trying to pass an argument byref
* Skip Fable.Core version check

### 2.1.10

* Fix #1698: Reflection info for delegates

### 2.1.9

* `TimeSpan` fixes (parsing, stringifying) @rfrerebe
* Fix `Int64.TryParse` @MangelMaxime
* Add `ResizeArray.Exists` @wangweipeng
* Fix #1687: Add `document` to JS keywords @MangelMaxime
* Fix #1683: Math.Truncate
* Fix #1689: List.pairwise
* Fix #1672: List.filter works backwards
* Fix #1637: Relax GUID parsing
* Fix #1536: Allow enums with unknown name and fix enum inlining


### 2.1.8

* Added LazyPattern operator @ncave #1669
* Fix #1667: cannot find inline type extensions in other files
* Fix order of files returned by Project.GetFilesAndDependent

### 2.1.7

* Fixed parsing to match F# behavior a bit closer @ncave #1665
* Fix #1523: Mark files with inlined functions as dependencies
* Fix inlined member with optional arguments

### 2.1.6

* Fix #1664: Reflection info for built-in Choice

### 2.1.5

* Fix #1662: Type aliases with same name in different modules of same file
* Throw error for global/imported members that are mutable and public

### 2.1.4

* Improve number parsing @ncave
* BigInt.TryParse
* Don't get reflection info for generic numbers into the runtime

### 2.1.3

* Fix #1658: Array.choose must construct array of output type @GBirkel

### 2.1.2

* Fix #1655: Result reflection

### 2.1.1

* Try not to stop compilation if `dotnet restore` fails.

### 2.1.0

* Release _stablish_ version

### 2.1.0-beta-007

* Use fable-babel-plugins package

### 2.1.0-beta-006

* Fix #1640: Nicer display name for function constructors

### 2.1.0-beta-005

* Add reflection info for each declared type

### 2.1.0-beta-003

* Rename to fable-compiler

### 2.1.0-beta-002

* Allow referencing .fs directly and other enhancements

### 2.1.0-beta-001

* Update assembly and set 2.1.0 as minimum runtime version

### 2.1.0-alpha-002

* Add babel-plugins
* Accept `path` argument to run Fable locally

### 2.1.0-alpha-001

* First release
