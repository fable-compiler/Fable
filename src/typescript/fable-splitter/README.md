# fable-splitter

Fable (F# to JavaScript compiler) file splitter.

## Installation

todo:

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
import fableSplitter from "fable-splitter";

const babelOptions = {
  plugins: [
    ["transform-es2015-modules-commonjs", { }],
  ],
  // presets: [
  //   ["es2015", { modules: false }],
  // ],
  //sourceMaps: true,
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

