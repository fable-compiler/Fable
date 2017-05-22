import fableCompiler from "./index";

// const Path = require("path");

// function resolve(filePath: string) {
//   return Path.resolve(__dirname, filePath);
// }

const babelOptions = {
  plugins: [
    ["transform-es2015-modules-commonjs", { }],
  ],
  // presets: [
  //   ["es2015", { modules: false }],
  // ],
};

const fableOptions = {
  fableCore: "../../../build/fable-core",
  // plugins: [],
  define: [
    "COMPILER_SERVICE",
    "FX_NO_CORHOST_SIGNER",
    "FX_NO_LINKEDRESOURCES",
    "FX_NO_PDB_READER",
    "FX_NO_PDB_WRITER",
    "FX_NO_WEAKTABLE",
    "NO_COMPILER_BACKEND",
    "NO_INLINE_IL_PARSER",
    "TRACE",
  ],
};

const options = {
  entry: "../../dotnet/Fable.Client.Browser/testapp/testapp.fsproj",
  outDir: "./out",
  // port: 61225,
  babel: babelOptions,
  fable: fableOptions,
};

fableCompiler(options);
