# fable-loader

[Webpack](https://webpack.js.org/) loader for [Fable](http://fable.io/) (F# to JS compiler)

## Installation

```npm install webpack webpack-cli @babel/core fable-loader```

## Usage

Create a `webpack-config.js` like the following:

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

You can then bundle your app by running `dotnet fable webpack-cli` in the directory where you have installed dotnet-fable tool. Check [Fable website](http://fable.io/) for more info.

You can use the fable-loader in combination with `babel-loader` to further transform JavaScript files (e.g., to make them compatible with older browsers). Check [their website](https://github.com/babel/babel-loader) for more info.

These are the options that can be passed to `fable-loader`:

- **babel**: [Babel options](https://babeljs.io/docs/en/options) (only applied when transforming F# files, you may want to use the `babel-loader` if you also have JS files in your project).
- **define**: Array of compilation constants passed to the F# compiler. Fable automatically defines `FABLE_COMPILER` and fable-loader will also define `DEBUG` in Webpack "development" mode.
- **typedArrays**: Translate numeric arrays as JS [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray). True by default.
- **clampByteArrays**: If true, Fable will translate byte arrays as [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray).
