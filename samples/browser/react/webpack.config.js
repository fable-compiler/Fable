var path = require("path");
module.exports = {
  context: path.join(__dirname, "public"), 
  entry:  "./client",
  output: {
    path: path.join(__dirname, "public"),
    filename: "bundle.js"
  },
  devtool: "source-map",
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        loader: "source-map-loader"
      }
    ]
  }
};