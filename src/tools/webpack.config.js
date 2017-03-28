var path = require('path');

function resolve(relativePath) {
    return path.join(__dirname, relativePath);
}

module.exports = {
  entry: resolve('./QuickTest.fsproj'),
  output: {
    filename: 'QuickTest.js',
    path: resolve('temp'),
  },
  module: {
    rules: [
      {
        test: /\.fs(proj|x)?$/,
        use: {
          loader: resolve("../typescript/fable-loader"),
          options: {
            define: ["DEBUG"],
            fableCore: resolve("../../build/fable-core"),
            // babel: babelOptions,
            extra: {
              saveAst: resolve("temp")
            }
          }
        }
      },
    //   {
    //     test: /\.js$/,
    //     exclude: /node_modules/,
    //     use: {
    //       loader: 'babel-loader',
    //       options: babelOptions
    //     },
    //   }
    ]
  },
  // externals: [
  //   function(context, request, callback) {
  //     if (/fable-core/.test(request)){
  //       return callback(null, 'commonjs ' + request);
  //     }
  //     callback();
  //   }
  // ]
};
