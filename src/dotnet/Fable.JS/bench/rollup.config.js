import fable from 'rollup-plugin-fable';
const fableUtils = require("fable-utils");
const path = require('path');

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

// const babelOptions = fableUtils.resolveBabelOptions({
//   "presets": [
//     // ["env", { "modules": false }],
//     // ["minify", { "mangle": false }],
//   ]
// });

const fableOptions = {
  //babel: babelOptions,
  fableCore: resolve("../../../../build/fable-core"),
  //plugins: [],
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

export default {
  input: resolve('./bench.fsproj'),
  output: {
    file: resolve('./out/bundle.js'),
    format: 'cjs', // 'amd', 'cjs', 'es', 'iife', 'umd'
  },
  //sourceMap: 'inline',
  plugins: [
    fable(fableOptions),
  ],
};
