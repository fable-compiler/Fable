# rollup-plugin-fable

Rollup plugin for [Fable](http://fable.io/) (F# to JS compiler)

## Installation

```npm install --save-dev babel-core rollup rollup-plugin-fable```

## Usage

Create a `rollup-config.js` like the following:

```js
import path from 'path';
import fableUtils from 'fable-utils';
import fable from 'rollup-plugin-fable';
import nodeResolve from 'rollup-plugin-node-resolve';

function resolve(relativePath) {
    return path.join(__dirname, relativePath);
}

var babelOptions = fableUtils.resolveBabelOptions({
  'presets': [
    ['es2015', {'modules': false}]
  ]
});

export default {
  entry: resolve('./my-app.fsproj'),
  dest: resolve('./dist/bundle.js'),
  plugins: [
    fable({ babel: babelOptions })
    nodeResolve({
      // Other node-resolve options here
      // See https://github.com/rollup/rollup-plugin-node-resolve
      customResolveOptions: {
          moduleDirectory: resolve('./node_modules')
      }
    })
  ],
  format: 'cjs'
};
```

> Note we're resolving paths as well as Babel options and node modules to prevent conflicts in case Fable pulls files from outside the project local directory (for example, from Nuget cache).

Add this to your [package.json](https://docs.npmjs.com/files/package.json).

```json
"scripts": {
  "rollup": "rollup -c rollup-config.js"
}
```

You can then bundle your app by running: `dotnet fable npm-run rollup`.

> Check [Fable website](http://fable.io/) for more info

Normally you'll also install `rollup-plugin-babel` for other JavaScript files in your project (including those in Fable.Core) and you'll share the Babel options between both plugins. Check [rollup-plugin-babel README](https://github.com/rollup/rollup-plugin-babel) and [Babel website](https://babeljs.io/docs/usage/api/#options) to find more info about the available options.


These are the options that can be passed to `rollup-plugin-fable`:

- **babel**: Babel options as mentioned above.
- **define**: Array of compiler directives passed to the F# compiler (like `DEBUG`). Note _Fable will ignore the `DefineConstants` property in .fsproj_.
- **plugins**: Array of paths to Fable plugins (.dll files).
- **typedArrays**: Translate numeric arrays as JS [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray). True by default.
- **clampByteArrays**: If true, Fable will translate byte arrays as [Uint8ClampedArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray).
- **fableCore**: Specify a directory containing Fable.Core JS files, normally used for testing new Fable versions.
