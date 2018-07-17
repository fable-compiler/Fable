const babelOptions = {
  plugins: [
    ["transform-es2015-modules-commonjs"],
  ],
};

module.exports = {
  entry: "./bench.js.fsproj",
  outDir: "./out",
  // port: 61225,
  babel: babelOptions,
};
