const path = require("path");

module.exports = {
  entry: path.join(__dirname, "QuickTest.fsproj"),
  outDir: path.join(__dirname, "temp"),
  fable: {
    define: ["DEBUG"]
  },
  babel: {
    plugins: ["transform-es2015-modules-commonjs"],
    // presets: [ ["env", {"modules": "commonjs"}] ]
  },
  // allFiles: true
};
