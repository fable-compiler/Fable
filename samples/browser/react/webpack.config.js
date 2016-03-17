var path = require("path");
var webpack = require("webpack");
var publicPath = path.join(__dirname, "public");

module.exports = {
  devtool: "source-map",
  entry: [
    "webpack-dev-server/client?http://localhost:8080",
    'webpack/hot/only-dev-server',
    "./public/client"
  ],
  output: {
    path: publicPath,
    publicPath: "/",
    filename: "bundle.js"
  },
  plugins: [
    new webpack.HotModuleReplacementPlugin()    
  ],
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        include: publicPath,
        loader: "source-map-loader"
      }
    ],
    loaders: [
      {
        test: /\.js$/,
        include: publicPath,
        loader: "fable-react-hot-loader"
      }
    ]
  },
  devServer: {
    hot: "true",
    contentBase: "public/",
    publicPath: "/",
    historyApiFallback: true,
    proxy: {
      '/api/*': {
        target: 'http://localhost:3000'
      }
    }
  }
};