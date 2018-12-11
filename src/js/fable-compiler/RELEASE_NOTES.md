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
