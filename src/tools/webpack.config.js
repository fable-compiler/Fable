var path = require('path');

function resolve(relativePath) {
    return path.join(__dirname, relativePath);
}

module.exports = {
  entry: resolve('./QuickTest.fsx'),
  output: {
    filename: 'QuickTest.js',
    path: path.resolve(__dirname, 'temp'),
  },
  module: {
    rules: [
      {
        test: /\.fs[proj|x]?$/,
        use: {
          loader: path.resolve(__dirname, "../typescript/fable-loader"),
        //   options: {
        //     define: ["DEBUG"],
        //     babel: babelOptions
        //   }
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
