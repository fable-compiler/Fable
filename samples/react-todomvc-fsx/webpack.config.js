module.exports = {
  entry: './src/ReactTodoMVC.fsx',
  mode: "development",
  devServer: {
      contentBase: "./dist",
      port: 8080,
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
          use: {
              loader: "fable-loader",
              options: {
                  cli: {
                      path: "../../src/Fable.Cli"
                  }
              }
          }
      }
    ]
  },
  externals: {
    "react": "React",
    "react-dom": "ReactDOM",
  },
};
