const path = require("path");

const useCommonjs = process.argv.find(v => v === "--commonjs");
console.log("Compiling to " + (useCommonjs ? "commonjs" : "ES2015 modules") + "...")

const babelOptions = useCommonjs
  ? { plugins: ["@babel/plugin-transform-modules-commonjs"] }
  : {};

const fableOptions = {
  define: [
    "FX_NO_BIGINT"
  ],
};

const outDir = useCommonjs
  ? "../../build/fable-library-commonjs"
  : "../../build/fable-library";

module.exports = {
  entry: path.join(__dirname, "Fable.Library.fsproj"),
  outDir: path.join(__dirname, outDir),
  allFiles: true,
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
