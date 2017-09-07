const path = require('path');
const fableUtils = require('fable-utils');

function resolve(relativePath) {
  return path.join(__dirname, relativePath);
}

var babelOptions = fableUtils.resolveBabelOptions({
  "presets": [
    ["es2015", {"modules": false}]
  ]
});

const config = {
  entry: resolve('./Main/Fable.Tests.fsproj'),
  output: {
    path: resolve('../build/tests'),
    filename: 'bundle.js'
  },
  resolve: {
    modules: [
      "node_modules", resolve("../node_modules/")
    ]
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
            loader: "fable-loader",
            options: {
                fableCore: resolve("../build/fable-core"),
                define: "DOTNETCORE",
                plugins: resolve("../build/nunit/Fable.Plugins.NUnit.dll"),
                babel: babelOptions
            }
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

module.exports = config;