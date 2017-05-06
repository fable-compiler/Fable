var path = require('path');

function resolve(relativePath) {
    return path.join(__dirname, relativePath);
}

var babelOptions = {
  "presets": [
    ["es2015", {"modules": false}]
  ]
}

module.exports = {
  entry: resolve('./QuickTest.fsx'),
  output: {
    filename: 'QuickTest.js',
    path: resolve('temp'),
  },
  target: "node",
  module: {
    rules: [
      {
        test: /\.fs(proj|x)?$/,
        use: {
          loader: "fable-loader",
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
