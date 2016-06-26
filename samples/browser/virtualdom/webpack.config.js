var path = require("path");
var webpack = require("webpack");
var cfg = {
  devtool: "source-map",
  entry: "./temp/virtualdom.js",
  output: {
    path: path.join(__dirname, "out"),
    filename: "bundle.js"
  },
  module: {
  }
};
//node node_modules/webpack/bin/webpack out/virtualdom.js out/bundle.js"
module.exports = cfg;
