const path = require("path");

const useCommonjs = process.argv.find(v => v === "--commonjs");
console.log("Compiling to " + (useCommonjs ? "commonjs" : "ES2015 modules") + "...")

const babelOptions = useCommonjs
  ? { plugins: ["@babel/plugin-transform-modules-commonjs"] }
  : {};

const fableOptions = {
  // define: [
  // ],
  // extra: { saveAst: "./ast" }
};

module.exports = {
  entry: path.join(__dirname, "./Worker.fsproj"),
  outDir: path.join(__dirname, "../../../../build/fable-standalone/out-worker"),
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
