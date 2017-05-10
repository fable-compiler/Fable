var path = require('path');

function resolve(relativePath) {
    return path.join(__dirname, relativePath);
}

var babelOptions = {
  "presets": [
    ["es2015", {"modules": false}]
  ]
}

module.exports = {
  entry: resolve('./React.TodoMVC.fsproj'),
  output: {
    filename: 'bundle.js',
    path: resolve('out'),
  },
  devtool: "source-map",
  module: {
    rules: [
      {
        test: /\.fs(proj|x)?$/,
        use: {
          loader: "fable-loader",
          options: {
            // define: ["DEBUG"],
            babel: babelOptions,
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
  },
  externals: {
    "react": "React",
    "react-dom": "ReactDOM"
  }
};
