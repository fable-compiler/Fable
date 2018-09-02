const path = require("path");

// const babelOptions = {
//   plugins: [
//     ["transform-es2015-modules-commonjs"],
//   ],
//   // presets: [
//   //   ["env", { modules: "umd" }],
//   // ],
//   // sourceMaps: true,
// };

const fableOptions = {
  define: [
    "FX_NO_BIGINT"
  ],
};

module.exports = {
  entry: path.join(__dirname, "Fable.Core.JS.fsproj"),
  outDir: path.join(__dirname, "../../../build/fable-core"),
  allFiles: true,
  // port: 61225,
  // babel: babelOptions,
  fable: fableOptions,
};
