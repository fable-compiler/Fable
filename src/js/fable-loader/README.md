# fable-loader

[Webpack](https://webpack.js.org/) loader for [Fable](http://fable.io/) (F# to JS compiler)

## Installation

```npm install webpack webpack-cli @babel/core fable-loader```

## Usage

Create a `webpack-config.js` like the following run it using [Webpack CLI](https://webpack.js.org/guides/getting-started/).

**ATTENTION**: In Fable 2.0 you had to call Webpack through dotnet-fable cli too (e.g. `dotnet fable webpack`), starting from Fable 2.1 you can call Webpack directly (e.g. `npx webpack`).

> See the [Fable webpack-config-template](https://github.com/fable-compiler/webpack-config-template) for a more comprehensive example of a Webpack configuration for Fable projects.

```js

var path = require("path");

module.exports = {
    mode: "production",
    entry: "./src/App.fsproj",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        contentBase: "./public",
        port: 8080,
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
}

```

## Options

These are the options that can be passed to `fable-loader`:

- **babel**: [Babel options](https://babeljs.io/docs/en/options) (only applied when transforming F# files, you may want to use the `babel-loader` if you also have JS files in your project).
- **define**: Array of compilation constants passed to the F# compiler. Fable automatically defines `FABLE_COMPILER` and fable-loader will also define `DEBUG` in Webpack "development" mode.
- **typedArrays**: Translate numeric arrays as JS [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray). True by default.
- **clampByteArrays**: If true, Fable will translate byte arrays as [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray).
