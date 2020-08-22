const path = require("path");
const portArgIndex = process.argv.indexOf("--port");

module.exports = {
  port: portArgIndex >= 0 ? process.argv[portArgIndex + 1] : undefined,
  noReferences: true,
  cli: { path: resolve("../Fable.Cli") },
  entry: resolve("QuickTest.fsproj"),
//   outDir: resolve("temp"),
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