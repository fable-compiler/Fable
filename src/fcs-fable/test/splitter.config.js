const path = require("path");

const useCommonjs = process.argv.find(v => v === "--commonjs");
console.log("Compiling to " + (useCommonjs ? "commonjs" : "ES2015 modules") + "...")

const babelOptions = useCommonjs
  ? { plugins: ["@babel/plugin-transform-modules-commonjs"] }
  : {};

const fableOptions = {
  define: [
    "FX_NO_CORHOST_SIGNER",
    "FX_NO_PDB_READER",
    "FX_NO_PDB_WRITER",
    "FX_NO_WEAKTABLE",
    "NO_EXTENSIONTYPING",
    "NO_INLINE_IL_PARSER"
  ],
  // extra: { saveAst: "./ast" }
};

module.exports = {
  entry: path.join(__dirname, "./fcs-fable-test.fsproj"),
  outDir: path.join(__dirname, "./out-test"),
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
