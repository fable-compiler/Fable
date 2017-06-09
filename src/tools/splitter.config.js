const path = require("path");
const fableSplitter = require("../../build/fable-splitter").default;

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

const babelOptions = {
  plugins: ["transform-es2015-modules-commonjs"],
//   presets: [["es2015", { modules: "umd" }]],
//   sourceMaps: true,
};

const fableOptions = {
  fableCore: resolve("../../build/fable-core"),
  plugins: [],
  define: ["DEBUG"]
};

const options = {
  entry: resolve('./QuickTest.fsx'),
  outDir: "./temp",
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};

fableSplitter(options);
