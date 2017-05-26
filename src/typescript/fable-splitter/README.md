# fable-splitter

File splitter for Fable (F# to JavaScript compiler).

## Setup

Add the following to your package.json:
```
  "scripts": {
    "splitter": "node splitter.config.js",
  },
  "devDependencies": {
    "fable-splitter": "latest",
  }
```

Create a `splitter.config.js` like that:

```
const fableSplitter = require("fable-splitter").default;

const babelOptions = {
  // -- add this to generate UMD modules
  // plugins: [
  //   ["transform-es2015-modules-umd"],
  // ],
  // -- or add this to transpile to ES5
  // presets: [
  //   ["es2015", { modules: "umd" }],
  // ],
  // -- add this to generate source maps
  // sourceMaps: true,
  // etc.
};

const fableOptions = {
  // fableCore: "./node_modules/fable-core",
  // plugins: [],
  // define: [],
  // etc.
};

const prepackOptions = {
  // etc.
};

const options = {
  entry: "./test.fsproj",
  outDir: "./out",
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
  // prepack: prepackOptions, // if added, fable ==> babel ==> prepack
};

fableSplitter(options);
```

## Usage

dotnet fable npm-run splitter
