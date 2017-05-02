var path = require('path');

function resolve(filePath) {
  return path.resolve(__dirname, filePath)
}

var babelOptions = {
  "presets": [
    [resolve("../../node_modules/babel-preset-es2015"), {"modules": false}]
  ]
}

module.exports = {
  entry: resolve('./pong.fsx'),
  output: {
    filename: 'bundle.js',
    path: resolve('./out'),
    publicPath: '/out',
  },
  devtool: "source-map",
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: resolve("../../../src/typescript/fable-loader"),
          options: {
            fableCore: resolve("../../../build/fable-core"),
            babel: babelOptions
          }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules\/(?!fable)/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      }
    ]
  }
};
