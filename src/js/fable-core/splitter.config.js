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
  entry: "Fable.Core.JS.fsproj",
  outDir: "../../../build/fable-core",
  allFiles: true,
  // port: 61225,
  // babel: babelOptions,
  fable: fableOptions,
};
