import fable from 'rollup-plugin-fable';
var path = require('path');

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

// var babelOptions = {
//   "presets": [
//     // ["es2015", { "modules": false }],
//     // ["minify", { "mangle": false }],
//   ]
// };

var fableOptions = {
  // babel: babelOptions,
  fableCore: resolve("../../../../build/fable-core"),
  //plugins: [],
  define: [
    "COMPILER_PUBLIC_API",
    "FX_NO_CORHOST_SIGNER",
    "FX_NO_LINKEDRESOURCES",
    "FX_NO_PDB_READER",
    "FX_NO_PDB_WRITER",
    "FX_NO_WEAKTABLE",
    "FX_REDUCED_EXCEPTIONS",
    "NO_COMPILER_BACKEND",
    "NO_INLINE_IL_PARSER"
  ]
};

export default {
  input: resolve('../Fable.JS.fsproj'),
  output: {
    file: resolve('./repl/bundle.min.js'),
    format: 'iife', // 'amd', 'cjs', 'es', 'iife', 'umd'
  },
  name: 'Fable',
  //sourceMap: 'inline',
  plugins: [
    fable(fableOptions),
  ],
};
