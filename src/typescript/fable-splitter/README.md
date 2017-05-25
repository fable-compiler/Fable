# fable-splitter

File splitter for Fable (F# to JavaScript compiler).

## Usage

Add the following to your package.json:
```
  "scripts": {
    "splitter": "node splitter.config.js",
  },
  "devDependencies": {
    "fable-splitter": "latest",
  }
```

Create a `splitter.config.js` like the following:

```
const fableSplitter = require("fable-splitter").default;

const babelOptions = {
  // -- add this for CommonJS modules
  // plugins: [
  //   ["transform-es2015-modules-commonjs", { }],
  // ],
  // -- add this to transpile to ES5
  // presets: [
  //   ["es2015", { modules: false }],
  // ],
  // -- add this to generate source maps
  // sourceMaps: true,
};

const fableOptions = {
  // fableCore: "../../../build/fable-core",
  // plugins: [],
  define: [ "TRACE" ],
};

const options = {
  entry: "./fable-test.fsproj",
  outDir: "./out",
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};

fableSplitter(options);
```

