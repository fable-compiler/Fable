var path = require('path');

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = {
  "presets": [
    [resolve("../../../../node_modules/babel-preset-es2015"), {"modules": false}],
    //[resolve("../../../../node_modules/babel-preset-babili"), {}]
  ]
}

module.exports = {
  entry: resolve('../Fable.Client.Browser.fsproj'),
  output: {
    filename: 'bundle.min.js',
    path: resolve('./repl'),
  },
  //devtool: "source-map",
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: resolve("../../../typescript/fable-loader"),
          options: {
            babel: babelOptions,
            fableCore: resolve("../../../../build/fable-core"),
            plugins: [],
            define: [
              "FX_NO_CORHOST_SIGNER",
              "FX_NO_LINKEDRESOURCES",
              "FX_NO_PDB_READER",
              "FX_NO_PDB_WRITER",
              "FX_NO_WEAKTABLE",
              "NO_COMPILER_BACKEND",
              "NO_INLINE_IL_PARSER",
              "TRACE"
            ]
          }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules\/(?!fable)/,
        use: {
          loader: '../../../../node_modules/babel-loader',
          options: babelOptions
        },
      }
    ]
  },
};
