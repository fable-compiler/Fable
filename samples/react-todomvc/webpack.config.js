module.exports = {
  entry: './src/React.TodoMVC.fsproj',
  mode: "development",
  devServer: {
      contentBase: "./dist",
      port: 8080,
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: "fable-loader"
      }
    ]
  },
  externals: {
    "react": "React",
    "react-dom": "ReactDOM",
  },
};
