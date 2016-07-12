var fs = require("fs");
var path = require("path");
var webpack = require("webpack");

var outDir = path.join(__dirname,"bundle/js");
if (!fs.existsSync(outDir)) {
  fs.mkdirSync(outDir);
}
fs.createReadStream(path.join(__dirname,"temp/main.js"))
  .pipe(fs.createWriteStream(path.join(__dirname,"bundle/js/main.js")));

module.exports = {
  devtool: "source-map",
  entry: "./temp/renderer",
  output: {
    path: path.join(__dirname, "bundle/js"),
    filename: "renderer.js"
  },
  module: {
    preLoaders: [
      { test: /\.js$/, exclude: /node_modules/, loader: "source-map-loader" }
    ]
  }
};
