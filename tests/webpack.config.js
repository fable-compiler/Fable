module.exports = {
  entry: './Main/Fable.Tests.fsproj',
  output: {
    path: '../build/tests',
    filename: 'bundle.js'
  },
  module: {
    rules: [
      {
        test: /\.fs(proj)?$/,
        use: {
            loader: "fable-loader",
            options: { define: "DOTNETCORE" }
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            "presets": [
              ["env", {"modules": false}]
            ]
          }
        },
      }
    ]
  }
};
