const path = require("path");

const useCommonjs = process.argv.find(v => v === "--commonjs");
console.log("Compiling to " + (useCommonjs ? "commonjs" : "ES2015 modules") + "...")

const babelOptions = useCommonjs
  ? { plugins: ["@babel/plugin-transform-modules-commonjs"] }
  : { };

const fableOptions = {
  typedArrays: true,
  define: [
    "FX_NO_BIGINT"
  ],
  noReferences: true
  // classTypes: true,
};

const outDir = useCommonjs
  ? "../../build/fable-library-commonjs"
  : "../../build/fable-library";

module.exports = {
  cli: {
    path: resolve("../Fable.Cli"),
    fableLibrary: "force:${outDir}",
    verbose: true
  },
  entry: resolve("Fable.Library.fsproj"),
  outDir: resolve(outDir),
  allFiles: true,
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};

function resolve(p) {
  return path.join(__dirname, p);
}