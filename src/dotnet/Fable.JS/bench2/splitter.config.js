module.exports = {
  entry: "./bench2.fsproj",
  outDir: "./out",
  babel: {
    plugins: ["transform-es2015-modules-commonjs"],
  },
};
