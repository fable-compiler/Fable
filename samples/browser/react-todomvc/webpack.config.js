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
  entry: resolve('./React.TodoMVC.fsproj'),
  output: {
    filename: 'bundle.js',
    path: resolve('./out'),
  },
  devtool: "source-map",
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: resolve("../../../src/typescript/fable-loader"),
          // options: { babel: babelOptions }
        }
      },
      // {
      //   test: /\.js$/,
      //   exclude: /node_modules\/(?!fable)/,
      //   use: {
      //     loader: 'babel-loader',
      //     options: babelOptions
      //   },
      // }
    ]
  },
  externals: {
    "react": "React",
    "react-dom": "ReactDOM"
  }
};
