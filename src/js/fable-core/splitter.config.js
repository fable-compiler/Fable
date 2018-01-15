const path = require("path");
const fableUtils = require("fable-utils");

module.exports = {
  entry: resolve("Fable.Core.JS.fsproj"),
  outDir: resolve("../../../build/fable-core"),
  allFiles: true,
  fable: {
    fableCore: "${outDir}"
  },
};

function resolve(filePath) {
  return path.join(__dirname, filePath);
}
