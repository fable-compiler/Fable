const resolve = (path) => require("path").join(__dirname, path);

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
  cli: { path: resolve("../../../Fable.Cli") },
  entry: resolve("./Worker.fsproj"),
  outDir: resolve("../../../../build/fable-standalone/out-worker"),
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};
