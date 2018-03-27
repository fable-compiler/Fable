const fableSplitter = require("fable-splitter").default;
const fableUtils = require("fable-utils");

const fableOptions = {
  fableCore: "../../../../build/fable-core",
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
  entry: "../Fable.JS.fsproj",
  outDir: "./out",
  fable: fableOptions,
};

fableSplitter(options);
