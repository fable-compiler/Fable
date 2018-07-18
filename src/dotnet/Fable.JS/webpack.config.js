const path = require("path");

module.exports = {
  mode: "production",
  devtool: "source-map",
  entry: "./out/Main.js",
  output: {
    path: path.join(__dirname, "bundle"),
    filename: "bundle.min.js",
    libraryTarget: 'var',
    library: 'Fable',
  },
  module: {
    rules: [{
      test: /\.js$/,
      use: {
        loader: 'babel-loader',
        options: { presets: [ ["env", { modules: false } ] ] },
      },
    }]
  }
};
