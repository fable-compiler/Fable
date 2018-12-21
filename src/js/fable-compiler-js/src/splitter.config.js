const path = require("path");

module.exports = {
  entry: path.join(__dirname, "./fable-compiler.fsproj"),
  outDir: path.join(__dirname, "../dist"),
  babel: {
    plugins: ["@babel/plugin-transform-modules-commonjs"],
  },
};
