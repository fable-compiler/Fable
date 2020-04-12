const resolve = (path) => require("path").join(__dirname, path);

const useCommonjs = process.argv.find(v => v === "--commonjs");
console.log("Compiling to " + (useCommonjs ? "commonjs" : "ES2015 modules") + "...")

const babelOptions = useCommonjs
  ? { plugins: ["@babel/plugin-transform-modules-commonjs"] }
  : {};

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
  cli: { path: resolve("../Fable.Cli") },
  entry: resolve("./src/Fable.Standalone.fsproj"),
  outDir: resolve("../../build/fable-standalone/out-bundle"),
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
