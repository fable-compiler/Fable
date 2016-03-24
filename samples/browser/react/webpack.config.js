var path = require("path");
var webpack = require("webpack");

var cfg = {
  devtool: "source-map",
  entry: "./out/client/client",
  output: {
    path: path.join(__dirname, "public"),
    filename: "bundle.js"
  },
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "source-map-loader"
      }
    ]
  }
};

if (process.env.WEBPACK_DEV_SERVER) {
    cfg.entry = [
        "webpack-dev-server/client?http://localhost:8080",
        'webpack/hot/only-dev-server',
        "./out/client/client"
    ];
    cfg.plugins = [
        new webpack.HotModuleReplacementPlugin()    
    ];
    cfg.module.loaders = [{
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "react-hot-loader"
    }];
    cfg.devServer = {
        hot: true,
        contentBase: "public/",
        publicPath: "/",
        historyApiFallback: true,
        proxy: {
            '/api/*': {
                target: 'http://localhost:3000'
            }
        }
    };
}

module.exports = cfg;
