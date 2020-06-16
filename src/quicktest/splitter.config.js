const path = require("path");

module.exports = {
  noReferences: true,
  cli: { path: resolve("../Fable.Cli") },
  entry: resolve("QuickTest.fsproj"),
//   outDir: resolve("temp"),
//   port: 61225,
  fable: {
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