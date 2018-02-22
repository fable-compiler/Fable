module.exports = {
  entry: "Main/Fable.Tests.fsproj",
  outDir: "../build/tests",
  fable: { define: ["DEBUG"] },
  babel: {
    plugins: ["transform-es2015-modules-commonjs"],
    // presets: [ ["env", {"modules": false}] ]
  },
  allFiles: true
};