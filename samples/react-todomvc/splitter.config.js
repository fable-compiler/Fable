module.exports = {
  entry: "QuickTest.fsproj",
  outDir: "temp",
  fable: {
    define: ["DEBUG"]
  },
  babel: {
    plugins: ["transform-es2015-modules-commonjs"],
    //   presets: [["env", { modules: "commonjs" }]],
  },
  // allFiles: true
};
