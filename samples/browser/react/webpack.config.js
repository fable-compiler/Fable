var path = require("path");
var publicPath = path.join(__dirname, "public");

module.exports = {
  context: publicPath, 
  entry: "./client",
  output: {
    path: publicPath,
    publicPath: "/",
    filename: "bundle.js"
  },
  devtool: "source-map",
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "source-map-loader"
      }
    ],
    loaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "fable-react-hot-loader"
      }
    ]
  },
  devServer: {
    contentBase: "public/",
    proxy: {
      '/api/*': {
        target: 'http://localhost:3000'
      }
    }
  }
};