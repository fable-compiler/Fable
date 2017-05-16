var path = require("path");
var webpack = require("webpack");

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

var babelOptions = resolveBabelOptions({
  presets: [["es2015", {"modules": false}]],
  plugins: ["transform-runtime"]
});

module.exports = {
  devtool: "source-map",
  entry: resolve('./FableTemplate.fsproj'),
  output: {
    filename: 'bundle.js',
    path: resolve('./public'),
  },
  resolve: {
    modules: [
      "node_modules", resolve("./node_modules/")
    ]
  },
  devServer: {
    contentBase: resolve('./public'),
    port: 8080
 },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
          loader: "fable-loader",
          options: { babel: babelOptions }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: babelOptions
        },
      }
    ]
  }
};

// UTILS ----------------

function resolve(filePath) {
  return path.join(__dirname, filePath)
}

// Resolve plugin paths to prevent errors with fable-core files
// See: https://github.com/webpack/webpack/issues/1866#issuecomment-203590582
function resolveBabelOptions(babelOptions) {
  function res(isPreset) {
    return function (option) {
      return Array.isArray(option)
        ? [require.resolve((isPreset ? "babel-preset-" : "babel-plugin-") + option[0]), option[1]]
        : require.resolve((isPreset ? "babel-preset-" : "babel-plugin-") + option);
    }
  }
  return Object.assign({}, babelOptions, {
    presets: babelOptions.presets.map(res(true)),
    plugins: babelOptions.plugins.map(res(false))
  });
}