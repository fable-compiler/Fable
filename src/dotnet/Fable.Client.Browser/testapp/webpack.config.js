var path = require('path');

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

var babelOptions = {
  "presets": [
    [resolve("../../../../node_modules/babel-preset-es2015"), {"modules": false}],
    //[resolve("../../../../node_modules/babel-preset-babili"), {}]
  ]
}

module.exports = {
  target: 'node',
  entry: resolve('./project.fsproj'),
  output: {
    filename: 'bundle.js',
    path: resolve('./out'),
    library: "Fable"
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
