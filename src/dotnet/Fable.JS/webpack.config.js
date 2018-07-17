const path = require("path");
const UglifyJSPlugin = require('uglifyjs-webpack-plugin');

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
  plugins: [
    // Inlining is causing problems in minified code
    // See https://github.com/mishoo/UglifyJS2/issues/2842#issuecomment-359527962
    new UglifyJSPlugin({
      uglifyOptions: { compress: { inline: 1 } }
    }),
  ]
};
