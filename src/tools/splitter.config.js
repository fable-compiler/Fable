const path = require("path");

module.exports = {
  entry: path.join(__dirname, "QuickTest.fsproj"),
  outDir: path.join(__dirname, "temp"),
  fable: {
    define: ["DEBUG"]
  },
  babel: {
    plugins: ["@babel/plugin-transform-modules-commonjs"],
    //   presets: [["@babel/preset-env", { modules: "commonjs" }]],
  },
  // allFiles: true
};
