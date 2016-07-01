var fs = require("fs");
var path = require("path");

var outDir = path.join(__dirname,"out");
if (!fs.existsSync(outDir)) {
  fs.mkdirSync(outDir);
}
fs.createReadStream(path.join(__dirname,"temp/main.js"))
  .pipe(fs.createWriteStream(path.join(__dirname,"out/main.js")));

module.exports = {
  devtool: "source-map",
  entry: "./temp/client",
  output: {
    path: path.join(__dirname, "out"),
    filename: "bundle.js"
  },
  module: {
    preLoaders: [
      { test: /\.js$/, exclude: /node_modules/, loader: "source-map-loader" }
    ],
    loaders: [
        { test: /\.css$/, loader: "style-loader!css-loader" }
    ]
  }
};
