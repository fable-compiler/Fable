const path = require("path");

module.exports = {
  cli: { path: resolve("../Fable.Cli"), verbose: true },
  entry: resolve("QuickTest.fsx"),
//   outDir: resolve("temp"),
  fable: {
    define: ["DEBUG"]
  },
  // allFiles: true
};

function resolve(p) {
    return path.join(__dirname, p);
}