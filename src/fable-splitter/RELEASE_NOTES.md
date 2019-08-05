### 2.1.10

* Add `onCompiled` hook

### 2.1.8

* Add back possibility to connect to TCP port

### 2.1.7

* Fix compilation of .fsx files @xdaDaveShaw

### 2.1.6

* Fix #1751: Prevent Babel from searching for config files when reading AST

### 2.1.5

* Accept fable-compiler 2.2.x dependency

### 2.1.2

* Revert change from #1684

### 2.1.1

* Pass Babel options from splitter.config

### 2.1.0

* Release _stablish_ version

### 2.1.0-beta-005

* Use fable-babel-plugins package

### 2.1.0-beta-004

* Use fable-compiler package

### 2.1.0-beta-003

* Fix #1646: Don't override `allFiles` in config file if cli flag is not set

### 2.1.0-beta-002

* Remove nodemon dependency

### 2.1.0-beta-001

* Add fable-compiler-dotnet dependency for standalone compilation

### 2.1.0-alpha-002

* Add `-d|--debug` flag to automatically define "DEBUG" constant
* Automatically add `@babel/plugin-transform-modules-commonjs` plugin when using `--run`

### 2.1.0-alpha-001

* Add `--run` flag to run script with node after compilation (compatible with `--watch`)

### 2.0.2

* Add "externals" option (similar to Webpack's but only for global values)

### 2.0.1

* Improve relative path check

### 2.0.0

* Fable 2

### 2.0.0-beta-002

* Compatibility with Babel 7

### 2.0.0-alpha-003

* Fable 2 alpha

### 0.1.20

* Move `allFiles` to options root

### 0.1.19

* Add `extra.allFiles` option (See Fable #1165)

### 0.1.18

* Show compile times #1153

### 0.1.15

* Validate options

### 0.1.14

* Compile also imports with absolute paths
* Fix TSLint errors
* Update README

### 0.1.10

* Add `postbuild` option

### 0.1.9

* Fixed line endings

### 0.1.8

* Fix #1101: Keep final slash in string literals with macros

### 0.1.7

* Don't modify `process.argv`

### 0.1.6

* Remove `omitExtension` option
* Fix error in watch compilations

### 0.1.5

* Don't omit JS extension in import statements by default
* Add macros for path resolution

### 0.1.4

* Fix some errors

### 0.1.3

* Add CLI
* Put files in entry directory in outDir's root
* Add watch mode

### 0.1.2

* Added prepack support

### 0.1.1

* Added .npmignore

### 0.0.4

* First publish
