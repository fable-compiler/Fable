const path = require("path");
const fableUtils = require("fable-utils");

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
  plugins: ["transform-es2015-modules-commonjs"],
  //   presets: [["es2015", { modules: "commonjs" }]],
  //   sourceMaps: true,
})

module.exports = {
  entry: resolve("QuickTest.fsproj"),
  outDir: resolve("temp"),
  babel: babelOptions,
  fable: {
    fableCore: resolve("../../build/fable-core"),
    define: ["DEBUG"]
  }
};
