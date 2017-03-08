const path = require('path');

function resolve(relativePath) {
  return path.resolve(__dirname, relativePath);
}

const config = {
  entry: resolve('./Main/Fable.Tests.fsproj'),
  output: {
    path: resolve('../../build/tests'),
    filename: 'bundle.js'
  },
  module: {
    rules: [{
        test: /\.fs(x|proj)?$/,
        use: {
            loader: resolve("../typescript/fable-loader"),
            options: {
                define: "DOTNETCORE",
                plugins: resolve("../../build/nunit/Fable.Plugins.NUnit.dll"),
            }
        }
    }]
  },
  // externals: [
  //   function(context, request, callback) {
  //     if (/fable-core/.test(request)){
  //       return callback(null, 'commonjs ' + request);
  //     }
  //     callback();
  //   }
  // ],
};

module.exports = config;