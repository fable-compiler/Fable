var path = require("path");

module.exports = {
  entry: './babel-standalone.js',
  output: {
    filename: 'babel-standalone.js',
    path: path.join(__dirname, 'out'),
    libraryTarget: "commonjs"
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        // exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [["env", { modules: false }]],
            sourceMaps: false,
          }
        },
      }
    ]
  },
};
