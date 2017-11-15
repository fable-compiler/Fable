// import babel from 'rollup-plugin-babel';
// import fable from 'rollup-plugin-fable';
// const fableUtils = require("fable-utils");
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

// const fableOptions = {
//   // babel: babelOptions,
//   fableCore: resolve("../../../../build/fable-core"),
//   //plugins: [],
//   define: [
//     "COMPILER_PUBLIC_API",
//     "FX_NO_CORHOST_SIGNER",
//     "FX_NO_LINKEDRESOURCES",
//     "FX_NO_PDB_READER",
//     "FX_NO_PDB_WRITER",
//     "FX_NO_WEAKTABLE",
//     "FX_REDUCED_EXCEPTIONS",
//     "NO_COMPILER_BACKEND",
//     "NO_INLINE_IL_PARSER"
//   ]
// };

export default {
  // input: resolve('../Fable.JS.fsproj'),
  input: resolve('./out/Main.js'),
  output: {
    file: resolve('./repl/build/bundle.js'),
    format: 'iife', // 'amd', 'cjs', 'es', 'iife', 'umd'
  },
  name: 'Fable',
  //sourceMap: 'inline',
  plugins: [
    // fable(fableOptions),
    // babel(babelOptions),
  ],
};
