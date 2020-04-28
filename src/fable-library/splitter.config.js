const path = require("path");

const useCommonjs = process.argv.find(v => v === "--commonjs");
console.log("Compiling to " + (useCommonjs ? "commonjs" : "ES2015 modules") + "...")

const babelOptions = useCommonjs
  ? { plugins: ["@babel/plugin-transform-modules-commonjs"] }
  : { };

const fableOptions = {
  define: [
    "FX_NO_BIGINT"
  ],
};

const outDir = useCommonjs
  ? "../../build/fable-library-commonjs"
  : "../../build/fable-library-js";

module.exports = {
  cli: {
    path: resolve("../Fable.Cli"),
    fableLibrary: "force:${outDir}"
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