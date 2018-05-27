const babelOptions = {
  plugins: [
    ["transform-es2015-modules-commonjs"],
  ],
  // presets: [
  //   ["env", { modules: "umd" }],
  // ],
  // sourceMaps: true,
};

const fableOptions = {
  // fableCore: "../../../../build/fable-core",
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
  entry: "../Fable.JS.fsproj",
  outDir: "./out",
  //port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
