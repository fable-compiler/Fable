const path = require("path");

module.exports = {
  cli: {
    // verbose: true,
    path: resolve("../Fable.Cli")
  },
  entry: resolve("QuickTest.fsproj"),
//   outDir: resolve("temp"),
//   port: 61225,
  fable: {
    noRestore: true,
    noReferences: true,
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