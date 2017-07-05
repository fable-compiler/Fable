const fableSplitter = require("fable-splitter").default;

const babelOptions = {
  plugins: [
    ["transform-es2015-modules-commonjs"],
  ],
  // presets: [
  //   ["es2015", { modules: "umd" }],
  // ],
  // sourceMaps: true,
};

const fableOptions = {
  fableCore: "../../../../build/fable-core",
  // plugins: [],
  define: [
    "COMPILER_SERVICE",
    "FX_NO_CORHOST_SIGNER",
    "FX_NO_LINKEDRESOURCES",
    "FX_NO_PDB_READER",
    "FX_NO_PDB_WRITER",
    "FX_NO_WEAKTABLE",
    "NO_COMPILER_BACKEND",
    "NO_INLINE_IL_PARSER",
    "TRACE",
  ],
};

const options = {
  entry: "./testapp.fsproj",
  outDir: "./out",
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};

fableSplitter(options);
