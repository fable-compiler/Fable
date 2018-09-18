const path = require("path");

module.exports = {
  entry: path.join(__dirname, "./bench2.fsproj"),
  outDir: path.join(__dirname, "./out"),
  babel: {
    plugins: ["transform-es2015-modules-commonjs"],
  },
};
