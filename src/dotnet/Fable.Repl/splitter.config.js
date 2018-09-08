const path = require("path");

const useES6 = process.argv.find(v => v === "--es6");
const useCommonjs = process.argv.find(v => v === "--commonjs");
console.log("Compiling to " + (useCommonjs ? "commonjs" : "ES2015 modules") + "...")

const babelOptions = useES6 ? { } // no plugins, default presets
  : useCommonjs ? { plugins: ["@babel/plugin-transform-modules-commonjs"] }
  : { presets: [ ["@babel/preset-env", { modules: false }] ] }; // Uglify-js requires ES5

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
};

module.exports = {
  entry: path.join(__dirname, "./Fable.Repl.fsproj"),
  outDir: path.join(__dirname, "./out"),
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
