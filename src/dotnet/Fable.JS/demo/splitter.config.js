const path = require("path");
const fableSplitter = require("fable-splitter").default;

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

const fableOptions = {
  fableCore: resolve("../../../../build/fable-core"),
  define: [
    "FX_NO_CORHOST_SIGNER",
    "FX_NO_LINKEDRESOURCES",
    "FX_NO_PDB_READER",
    "FX_NO_PDB_WRITER",
    "FX_NO_WEAKTABLE",
    "FX_REDUCED_EXCEPTIONS",
    "NO_COMPILER_BACKEND",
    "NO_EXTENSIONTYPING",
    "NO_INLINE_IL_PARSER"
  ],
};

const options = {
  entry: resolve("../Fable.JS.fsproj"),
  outDir: resolve("./out"),
  fable: fableOptions,
};

console.log("Fable REPL build options", options);

fableSplitter(options);
