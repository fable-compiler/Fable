var fableUtils = require("fable-utils");
var path = require('path');

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
  "presets": [
    ["es2015", {"modules": false}]
  ]
});

var fableOptions = {
  babel: babelOptions,
  fableCore: resolve("../../../../build/fable-core"),
  plugins: [],
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

module.exports = {
  target: 'node',
  //devtool: "source-map",
  entry: resolve('./testapp.fsproj'),
  output: {
    filename: 'bundle.min.js',
    path: resolve('./out')
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: resolve("../../../js/fable-loader"),
          options: fableOptions
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      }
    ]
  },
};
