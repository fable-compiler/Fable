# fable-splitter

JS client for [Fable](http://fable.io/) that compiles F# files to individual JS files.

## Installation

```npm install --save-dev fable-splitter```

## Usage

```txt
fable-splitter [command] [arguments]

Commands:
  -h|--help         Show help
  --version         Print version
  [file path]       Compile an F# project or script to JS

Arguments:
  -o|--outDir       Output directory
  -c|--config       Config file
  -w|--watch        [FLAG] Watch mode
  -d|--debug        [FLAG] Define DEBUG constant
  --run             [FLAG] Run script with node after compilation

Example: fable-splitter src/App.fsproj -o dist/
```

> You can use node, npm scripts, yarn or npx to run the tool.

There are more options available using the JS API. For this, you can create a config file like the following:

```js
module.exports = {
  entry: "src/App.fsproj",
  outDir: "out",
  babel: {
    presets: [["env", { modules: "commonjs" }]],
    sourceMaps: false,
  }
}
```

These are the options that can be passed to `fable-splitter` through the JS API:

- **entry**: F# project entry file (`.fsproj` or `.fsx`).
- **outDir**: Output directory where JS files must be saved. Current directory will be used if not specified.
- **allFiles**: Compiles all project files even if some are not referenced (default `false`).
- **babel**: Babel options, check [Babel website](https://babeljs.io/docs/usage/api/#options) to find more.
- **fable**: Options to be passed to Fable:
  - **define**: Array of compiler directives passed to the F# compiler (like `DEBUG`). Note _Fable will ignore the `DefineConstants` property in .fsproj_.
  - **typedArrays**: Translate numeric arrays as JS [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray). True by default.
  - **clampByteArrays**: If true, Fable will translate byte arrays as [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray).

## Path resolution

For paths pointing to resources other than F# or JS files, you can use the `${entryDir}` and `${outDir}` macros to make sure the final JS file will contain a relative path that can be resolved properly:

```fsharp
let imgPath = "${entryDir}/../images/foo.png"
```

- `${entryDir}` resolves to the directory of the F# entry file (usually the `.fsproj`).
- `${outDir}` resolves to the directory set as `outDir` in fable-splitter options.
