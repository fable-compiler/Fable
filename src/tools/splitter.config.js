const path = require("path");

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

module.exports = {
  entry: resolve("QuickTest.fsx"),
  outDir: resolve("temp"),
  babel: {
    plugins: ["transform-es2015-modules-commonjs"],
    //   presets: [["es2015", { modules: "commonjs" }]],
    //   sourceMaps: true,
  },
  fable: {
    fableCore: resolve("../../build/fable-core"),
    define: ["DEBUG"]
  }
};
