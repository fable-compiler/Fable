# fable-compiler

[Fable](http://fable.io/) (F# to JS compiler)

## Installation

```npm install fable-compiler```

## Usage

Create a `webpack-config.js` like the following:

> See the [Fable webpack-config-template](https://github.com/fable-compiler/webpack-config-template) for a more comprehensive example of a Webpack configuration for Fable projects.

```js
const path = require("path");

let babelOptions = {
  plugins: ["@babel/plugin-transform-modules-commonjs"],
};

const fableOptions = {
  fableCoreDir: "",
  define: [
    "FABLE_COMPILER"
  ],
};

module.exports = {
  entry: path.join(__dirname, "./compiler.fsproj"),
  outDir: path.join(__dirname, "./out"),
  babel: babelOptions,
  fable: fableOptions,
};
```

You can then bundle your app by running `dotnet fable webpack-cli` in the directory where you have installed dotnet-fable tool. Check [Fable website](http://fable.io/) for more info.


These are the options that can be passed to `fable-compiler`:

- **babel**: [Babel options](https://babeljs.io/docs/en/options) (only applied when transforming F# files, you may want to use the `babel-loader` if you also have JS files in your project).
- **define**: Array of compilation constants passed to the F# compiler. Fable automatically defines `FABLE_COMPILER` and fable-loader will also define `DEBUG` in Webpack "development" mode.
- **typedArrays**: Translate numeric arrays as JS [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray). True by default.
- **clampByteArrays**: If true, Fable will translate byte arrays as [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray).
