const path = require("path");
let babelOptions = {};

if (process.argv.find(v => v === "--commonjs")) {
  babelOptions = {
    plugins: ["transform-es2015-modules-commonjs"],
  };
  console.log("Compiling to commmonjs...");
} else {
  console.log("Compiling to ES2015 modules...");
}

const fableOptions = {
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
  // extra: { saveAst: "./ast" }
};

module.exports = {
  entry: path.join(__dirname, "./bench.fsproj"),
  outDir: path.join(__dirname, "./out"),
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
