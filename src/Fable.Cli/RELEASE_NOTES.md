### 3.7.0-beta-001

* Add precompile command

### 3.6.3

* New FSharp.Core 6 APIs: updateAt/insertAt/insertManyAt/removeAt/removeManyAt
* Support thousand separators in String.Format
* Fix #2628 @stroborobo

### 3.6.2

* TypescriptTaggedUnion @cannorin
* Speed up recompilation when adding/removing files

### 3.6.1

* Fix #2614: Char addition
* Fix #2615: Math.DivRem
* Fix string template literals
* Improve Regex.Match/IsMatch/Matches
* Update FCS

### 3.6.0

* Support F# 6
* Support DateOnly/TimeOnly @kerams
* Improve watch mode
* Cache project options

### 3.6.0-beta-003

* Add Fable.Core.Compiler.triggeredByDependency flag
* Support DefaultParameterValue attribute (not for JS interop)
* Update F# compiler

### 3.6.0-beta-002

* Fix watch mode when saving multiple files at same time
* TimeOnly.FromDateTime @kerams
* Remove --watchDeps flag

### 3.6.0-beta-001

* Support F# 6
* Support DateOnly/TimeOnly @kerams
* Improve watch mode
* Add `--watchDeps` flag
* Cache project options

### 3.4.10

* Add support for StringSplitOptions.TrimEntries @steveofficer
* Fix #2587: DateTimeOffset.Parse issues with some locales @ncave
* Use Process.ArgumentList to escape args passed to subprocess
* Print paths relative to --cwd if set

### 3.4.9

* Add CLI arg --watchDelay
* Show relative paths in logs
* Fixed Seq.toArray @ncave
* Fix FullName/Name/Namespace of complex array types

### 3.4.8

* Fix #2572 #2579: Watch .fsi files and referenced .fsproj
* Fix #2576: Last file can omit module declaration
* Fix Seq.cache with partial enumeration
* Fix DateTime Offset parsing with date only and hyphens
* Set NODE_ENV when running a sub-process

### 3.4.7

* Fix #2571: Forward slash not escaped when creating regex

### 3.4.6

* Small improvements in Async.ts

### 3.4.5

* Accept ${entryDir} macro in imports

### 3.4.4

* Use relative paths for source maps

### 3.4.3

* Add support of System.Activator for primitive types @Happypig375
* Fix #2566: HashSet.IntersectWith does not respect custom comparer
* Fix calling super from an override when the method was declared in grandparent
* Compile to RegexConstant when possible
* Pass Fable compiled name to ReflectedDecorator
* Remove void wrapper in expression statements

### 3.4.2

* Fix #2561: Case insensitive args and check
* Fix generic parameters with JS.ReflectedDecorator

### 3.4.1

* Support System.Collections.Generic.Queue @davedawkins
* Fix custom array equality
* Remove class restriction for ParamObject
* Skip parens in emit if placeholder is already surrounded by parens

### 3.4.0

* ParamObject attribute
* Rename .fable folder to fable_modules
* Print log about Femto
* Add Type.IsInstanceOfType @chkn
* Fix #2229: Parsing succeeded for some invalid dates @njlr

### 3.3.1

* Fix #2097: Async.StartChild does not apply timeout @njlr
* Fix #2530: System.Collections.Generic.Stack @njlr
* Fix #2548: Assigning to value of unit type
* Fix #2549: Native ESM support
* Serialize compiler_info as JSON

### 3.3.0

* JS.Decorator/ReflectedDecorator attributes
* Fix isSubclassOf to walk up the inheritance chain @chkn
* Fix #2520: Uri.OriginalString @njlr
* Fix #2532: Measure products
* Optimize interpolate strings without formatting into JS templates

### 3.2.14

* Fix #2480: Include package.json in fable-library
* Fix #2522: Warn if user sets .fable as outDir
* Fix #2525: Support infinite arity for currying/uncurrying
* Fix plugin version check

### 3.2.12

* Fix #2505: Make String.Split match .NET with no or null separators
* Add TimeSpan.Divide and Multiply @0x53A
* Add Async.Sequential @0x53A
* Compile `FormattableString` as JS Templates

### 3.2.11

* Add --rootModule CLI flag

### 3.2.10

* Support System.Uri.TryCreate @Choc13
* Fix #2477: Don't drop "DEBUG" and "TRACE" DefineConstants @stroborobo
* Fix #2480: Improve tree shaking with fable-libray
* Fix #2485: Proper representation of Choice with 3 or more items
* Fix #2491: Unchecked.defaultof with struct tuples
* Fix #2496: Custom Pow operator

### 3.2.9

* Don't print JS files in watch mode if there're F# errors
* Fix SRTP with local inline functions

### 3.2.8

* Fix regression in FCS: passing __SOURCE_IDENTIFIER__ to static parameters in type providers

### 3.2.7

* Fix regression: all files were recompiled in watch mode in 3.2.5
* Fix #2472: Tuple-related methods

### 3.2.6

* Fix #2471: Trait call regression

### 3.2.5

* Fix #2468: SRTP Parser
* Only show Compile file log in watch compilations

### 3.2.4

* Fix #2438: Print JS sequence expressions always between parentheses
* Don't jump over mutable idents when inlining

### 3.2.3

* Experimental.namesofLambda

### 3.2.2

* Check for correct types in Anonymous Record when assigning to Interface with [<EmitIndexer>] via !! @Booksbaum
* Fix #1973: FormattableString support

### 3.2.1

* Fix Event issues and and implement FSharpEvent`2 @chkn
* Fix #2451: throw exception when sequence is empty @MNie
* Fix #2445: Improve error message when fable doesn't implement an API

### 3.2.0

* Update to net5.0 and FCS, @ncave

### 3.1.16

* Publish with icon and symbols @cartermp

### 3.1.15

* Add a --sourceMapsRoot CLI option to set source maps sourceRoot @mlaily
* Fix #2433: Improve type info for plugins like Fable.SvelteStore
* Fix #2431: Pass output directory info to plugins

### 3.1.14

* Experimental `casenameWithFieldIndex`

### 3.1.12

* Fix #1678: BigInt native JS JSON serialization with toJSON
* Fix #2151: Implement DateTimeOffset.toOffset @Booksbaum
* Fix #2410: Invalid offsets are accepted @Booksbaum
* Fix #2411: DateTime(Unspecified) fails when offset not local timezone @Booksbaum
* Fix #2417: overloads with struct tuple
* Fix #2418: List native JS JSON serialization with toJSON
* Update big.js (decimals) @ncave
* Update source-map-sharp to 1.0.5

### 3.1.11

* Fix watch compilation issues
* Fix #2398: two successive string format placeholders and value of first one ends in `%`

### 3.1.10

* Revert breaking change, configuration should default to Debug only in watch mode @forki

### 3.1.9

* Fix crash with delegate alias

### 3.1.8

* Fix #2234: Recompile dependencies in watch mode when Emit/Import attributes change
* Fix #2406: Check --outDir argument when running clean command

### 3.1.7

* Fix for Fable.Core.JsInterop.importValueDynamic

### 3.1.6

* Support setting a Build configuration with --configuration cli arg @stroborobo
* Log compiled files in same line
* Fix #2390: Array.choose is executing choose function two times @ncave
* Fix #2394: interpolate doesn't unescape %% correctly @thinkbeforecoding
* Fix #2396: custom exceptions in async workflows
* Fix #2400: Conversion to delegates
* Fix: Accessing named capture group in Regex only works with string constant @Booksbaum
* Fable library improvements and other fixes @ncave

### 3.1.5

* Fix #2384: Polling file watcher @mlaily
* Fix static constructors with attached members

### 3.1.4

* Fix #2045: Aliasing a function wrapping a multi-arity function in point-free style

### 3.1.3

* Add support for named capture groups in Regexes @Booksbaum
* Babel AST: cleanup and refactor @dbrattli
* Fix #1343: Warning when interface name clashes with record field @BillHally
* Fix #2376: Char.IsLetter with “ and ” @ncave @NickDarvey
* Fix #2372: Array.reduce Array.append @inosik
* Fix #2367: Using "unbox null" for callback returns "undefined"
* Fix #2357: Fall-through to default switch case duplicates the switch case consequent code block
* Fix #2356: Curried functions mangled via DU, List.fold and match combination
* Fix #2355: System.Math.Ceiling/Floor returning incorrect values for some Decimals @ncave
* Fix #2350: Empty files
* Fix #2116: Incorrect uncurrying with list of function options
* Fix #2047: Iterating over a list of functions with a for in loop
* Fix #2046: Assigning a function to scoped mutable variable
* Fix #2045: Raise warning for point-free function declarations

### 3.1.2

* Fast copy for typed arrays @GordonBGood
* Return error exit code when wront arguments passed
* Fix source map writing
* Fix #2350: Invalid JS class for empty files
* Fix #2355: System.Math.Ceiling() and System.Math.Floor returning incorrect values for some Decimals @ncave
* Fix #2357: Remove duplicate switch branches

### 3.1.1

* Fix #2343: Remove conflicting export default
* Fix #2336: Parameretized padding
* Fix reflection for int64/decimal with units of measure

### 3.1.0

* Source map support @delneg
* Fix #2342: recompile union tests in watch mode @tomcl
* Fix #2332: watch broken for certain directory structures @jwosty

### 3.1.0-beta-001

* Source map support @delneg
* Fix #2332: watch broken for certain directory structures @jwosty

### 3.0.5

* Fixed compiler option parsing @ncave
* Fix #2329: Class getters with import function helpers
* Fix #2328: Check process is available in context
* Fix #2327: Inherit global class in nested module
* Fix #2325: `AttachMembers` attribute

### 3.0.4

* Opt-in polling watcher @mlaily
* Fix #2323: Uncurrying Emit obj args

### 3.0.3

* Fix #2318: char ranges @inosik
* Fix #2320: `--yes` CLI arg
* Fix (partially) #2321: `System.Type.GetInterface`

### 3.0.2

* Fix #2313: Measure abbreviations
* Fix #2311: Arrow function printing

### 3.0.1

* Fix spread wrapped by parens @do-wa
* Add runtime check for Option.Value
* Remove equals/compare "fast" helper (sometimes fails)

### 3.0.0

* Official release
* Fix #2293: Fix case of path passed as cli argument @tomcl
* Fix #2277: Make FsWatcher case-insensitive @reinux
* Fix local imports with `import` function helpers

### 3.0.0-nagareyama-rc-011

* Fix #2303: spreading complex expressions
* Fix #2302: Fable.JsonProvider

### 3.0.0-nagareyama-rc-010

* Moar beta reduction improvements
* Use caching only in watch mode
* Ignore base constructor calls causing errors @ncave
* Fix watch dependencies when order of union cases changes
* Fix #2295: Disable errors affecting Fable 2-compliant code

### 3.0.0-nagareyama-rc-009

* Improve lambda beta reduction
* Fix #1962: Find entities by core assembly name
* Fix #2283: FABLE_COMPILER_3 constant
* Fix #2284: Inline import can absorb arguments
* Fix #2288: Ignore --warnaserror from project references
* Fix #2291: Default hashing for classes

### 3.0.0-nagareyama-rc-008

* Fix FSharpType.IsTuple for array of tuples @kerams
* In watch mode, recompile all files if F# failed previously
* Fix #2281: Hashing JS pojos @inosik
* Fix #2280: Inlined imports
* Fix #2278: Optimize `createObj`
* Fix #2276: Formatting Decimals

### 3.0.0-nagareyama-rc-007

* Always lower case ToString

### 3.0.0-nagareyama-rc-006

* Fix #2136: Type Provider invalidation @zanaptak
* Support Nullables

### 3.0.0-nagareyama-rc-005

* Fix #2267: Allow direct console output preserving colors @zanaptak
* Fix #2259: File conflict in outDir
* Fix #2260: Add new files to watcher
* StringBuilder.Append overloads @gusty
* Make file watcher more robust

### 3.0.0-nagareyama-rc-004

* LeanWork.IO.FileSystem.Watcher by Peter Meinl

### 3.0.0-nagareyama-rc-003

* Fix #1962: FSharp.UMX measure annotated types
* Fix #2250: Math.Clamp
* Fix #2257: Remove warnings in keyValueList

### 3.0.0-nagareyama-rc-002

* Patch FCS witness error @ncave
* Set always --langversion:preview

### 3.0.0-nagareyama-rc-001

* Release candidate
* FCS witnesses!
* Other fixes

### 3.0.0-nagareyama-beta-005

* Fix #2238: Normalize paths on Windows
* Fix #2212 (3rd attempt): Killing subprocess on Windows

### 3.0.0-nagareyama-beta-004

* Typescript-related updates @ncave
* Omit module prefix in imports @Zaid-Ajaj
* Write compiler options to .fable/compiler_info.txt
* Compatibility with Fable.AST beta-002

### 3.0.0-nagareyama-beta-003

* Fix #2226: Wrong decimal separator depending on regional setting @bklop
* Fix #2228: Remove unused values generated by imports meant for side effects
* Fix #2224: React.ofImport
* Fix #2216: Remove empty else blocks
* Fix #2212: Ctrl+C doesn't kill subprocess in Windows

### 3.0.0-nagareyama-beta-002

* Compatibility with Fable.AST beta-001

### 3.0.0-nagareyama-beta-001

* Beta release
* Fix .fsx compilation

### 3.0.0-nagareyama-alpha-017

* Only use cached (precompiled) files in debug mode

### 3.0.0-nagareyama-alpha-016

* Only use cached (precompiled) files in debug mode
* Optimize list construction
* Compatibility with latest Fable.AST

### 3.0.0-nagareyama-alpha-015

* Fix #2211: File extension in dynamic imports @Zaid-Ajaj
* Fix #2212: Kill subprocess in Windows @Zaid-Ajaj
* Fix #2213: Weird logs
* Compatibility with latest Fable.AST release
* Minor improvements in code generation

### 3.0.0-nagareyama-alpha-014

* Fixed entity resolution @ncave
* Cli improvements

### 3.0.0-nagareyama-alpha-012

* Fix plugins

### 3.0.0-nagareyama-alpha-011

* Plugins

### 3.0.0-nagareyama-alpha-010

* Fix #2202: Using System.Type as Dictionary key @Zaid-Ajaj
* Fix #2203: Comparing large sets @Zaid-Ajaj
* Keep only entity refs in Fable AST

### 3.0.0-nagareyama-alpha-009

* Detect if command after `--run` is in `node_modules/.bin` dir
* Set typedArrays=true as default again

### 3.0.0-nagareyama-alpha-008

* Use latest FCS @ncave
* Disable file caching when compiler version changes
* Add back base class for Union/Record
* Fix calls to static members of imported classes

### 3.0.0-nagareyama-alpha-007

* --runScript option
* Fix uncurrying of Emit calls
* Fix type testing performance
* Fix union/record string formattin

### 3.0.0-nagareyama-alpha-006

* Fix running process

### 3.0.0-nagareyama-alpha-005

* Fix watcher
* Use latest implementation of FSharpMap and FSharpSet
* Fix generic equality

### 3.0.0-nagareyama-alpha-004

* Improve CLI and other fixes

### 3.0.0-nagareyama-alpha-003

* Fix: Add names with $reflection suffix to root scope
* Fix deriving from imported JS classes
* Keep support for ParamList attribute until removed from Fable.React

### 3.0.0-nagareyama-alpha-002

* Ignore warnings from packages (as before)

### 3.0.0-nagareyama-alpha-001

* Nagareyama alpha
