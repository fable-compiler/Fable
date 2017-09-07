# fable-loader

[Webpack](https://webpack.js.org/) loader for [Fable](http://fable.io/) (F# to JS compiler)

## Installation

```npm install --save-dev webpack babel-core fable-loader```

## Usage

Create a `webpack-config.js` like the following:

```js
var path = require("path");
var fableUtils = require("fable-utils");

function resolve(relativePath) {
    return path.join(__dirname, relativePath);
}

var babelOptions = fableUtils.resolveBabelOptions({
  "presets": [
    ["es2015", {"modules": false}]
  ]
});

module.exports = {
  entry: resolve('src/MyProject.fsproj'),
  output: {
    filename: 'bundle.js',
    path: resolve('build/'),
  },
  resolve: {
    modules: [resolve("node_modules/")]
  },
  module: {
    rules: [
      {
        test: /\.fs(proj|x)?$/,
        use: {
          loader: "fable-loader",
          options: {
            define: ["DEBUG"],
            babel: babelOptions
          }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      }
    ]
  }
};
```

> Note we're resolving paths as well as Babel options and node modules to prevent conflicts in case Fable pulls files from outside the project local directory (for example, from Nuget cache).

Add this to your [package.json](https://docs.npmjs.com/files/package.json).

```json
"scripts": {
  "build": "webpack"
}
```

You can then bundle your app by running: `dotnet fable npm-run build`.

> Check [Fable website](http://fable.io/) for more info

As you can see in the sample, normally you'll install `babel-loader` for other JavaScript files in your project (including those in Fable.Core) and you'll share the Babel options between both loaders. Check [Babel website](https://babeljs.io/docs/usage/api/#options) to find more info about the available options.

These are the options that can be passed to `fable-loader`:

- **babel**: Babel options as mentioned above.
- **define**: Array of compiler directives passed to the F# compiler (like `DEBUG`). Note _Fable will ignore the `DefineConstants` property in .fsproj_.
- **plugins**: Array of paths to Fable plugins (.dll files).
- **typedArrays**: Translate numeric arrays as JS [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray). True by default.
- **clampByteArrays**: If true, Fable will translate byte arrays as [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray).
- **fableCore**: Specify a directory containing Fable.Core JS files, normally used for testing new Fable versions.
