module.exports = {
  entry: "./temp/index",
  output: {
    filename: "bundle.js",
    path: "./out"
  },
  devtool: "source-map",
  module: {
    preLoaders: [{
      loader: "source-map-loader",
      exclude: /node_modules/,
      test: /\.js$/
    }]
  }
};