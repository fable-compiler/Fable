const path = require("path");

module.exports = {
  cli: { path: resolve("../Fable.Cli") },
  entry: resolve("QuickTest.fsproj"),
//   outDir: resolve("temp"),
//   port: 61225,
  fable: {
    classTypes: true,
    define: [] //["DEBUG"]
  },
  babel: {
    sourceMaps: true
  }
  // allFiles: true
};

function resolve(p) {
    return path.join(__dirname, p);
}