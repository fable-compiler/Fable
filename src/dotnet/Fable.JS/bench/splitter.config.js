const fableUtils = require("fable-utils");

const babelOptions = fableUtils.resolveBabelOptions({
  plugins: [
    ["transform-es2015-modules-commonjs"],
  ],
  // presets: [
  //   ["env", { modules: "umd" }],
  // ],
  // sourceMaps: true,
});

const fableOptions = {
  // fableCore: "./.fable/fable-core.1.3.7",
  // plugins: [],
  define: [
    "FX_NO_CORHOST_SIGNER",
    "FX_NO_LINKEDRESOURCES",
    "FX_NO_PDB_READER",
    "FX_NO_PDB_WRITER",
    "FX_NO_WEAKTABLE",
    "FX_REDUCED_EXCEPTIONS",
    "NO_COMPILER_BACKEND",
    "NO_EXTENSIONTYPING",
    "NO_INLINE_IL_PARSER"
  ],
};

module.exports = {
  entry: "./bench.fsproj",
  outDir: "./out",
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
