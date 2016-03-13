module.exports = {
  context: __dirname + "/public", 
  entry:  "./client",
  output: {
    path: __dirname + "/public",
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