var path = require("path");
var webpack = require("webpack");

var cfg = {
  devtool: "source-map",
  entry: "./out/mario.js",
  output: {
    path: path.join(__dirname, "public"),
    filename: "bundle.js"
  }
};

module.exports = cfg;