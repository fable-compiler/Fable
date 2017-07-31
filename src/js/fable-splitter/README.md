# fable-splitter

JS client for [Fable](http://fable.io/) that compiles F# files to individual JS files.

## Installation

```npm install --save-dev fable-splitter babel-core```

## Usage

Add the following to the `scripts` section in your package.json:

```json
"scripts": {
  "build": "fable-splitter src/MyProject.fsproj --outDir out"
}
```

> You can also add `--watch` for watch mode. Type `./node_modules/.bin/fable-splitter --help` to see the available CLI arguments.

You can then compile your app by running: `dotnet fable npm-build`. Check [Fable website](http://fable.io/) for more info.

There are more options available using the JS API. For this, you can create a config file like the following:

```js
module.exports = {
  entry: "src/MyProject.fsproj",
  outDir: "out",
  babel: {
    presets: [["es2015", { modules: "commonjs" }]],
    sourceMaps: true,
  },
  fable: {
    define: ["DEBUG"]
  }
}
```

Then modify your build script as follows:

```json
"scripts": {
  "build": "fable-splitter --config splitter.config.js"
}
```

These are the options that can be passed to `fable-splitter` through the JS API:

- **entry**: F# project entry file (`.fsproj` or `.fsx`).
- **outDir**: Output directory where JS files must be saved. Current directory will be used if not specified.
- **port**: Fable daemon port (61225 by default).
- **omitExtension**: Omit `.js` extension in import statements.
- **fable**: Options to be passed to Fable:
  - **define**: Array of compiler directives passed to the F# compiler (like `DEBUG`). Note _Fable will ignore the `DefineConstants` property in .fsproj_.
  - **plugins**: Array of paths to Fable plugins (.dll files). See [Fable docs](http://fable.io/docs/plugins.html) for more info.
  - **typedArrays**: Translate numeric arrays as JS [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray). True by default.
  - **clampByteArrays**: If true, Fable will translate byte arrays as [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray).
  - **fableCore**: Specify a directory containing Fable.Core JS files, normally used for testing.
- **babel**: Babel options, check [Babel website](https://babeljs.io/docs/usage/api/#options) to find more.

## Path resolution

For paths pointing to resources other than F# or JS files, you can use the `${entryDir}` and `${outDir}` macros to make sure the final JS file will contain a relative path that can be resolved properly:

```fsharp
let imgPath = "${entryDir}/../images/foo.png"
```

- `${entryDir}` resolves to the directory of the F# entry file (usually the `.fsproj`).
- `${outDir}` resolves to the directory set as `outDir` in fable-splitter options.
