module.exports = {
  entry: './src/React.TodoMVC.fsproj',
  module: {
    rules: [
      {
        test: /\.fs(proj)?$/,
        use: { loader: "fable-loader" }
      }
    ]
  },
  externals: {
    "react": "React",
    "react-dom": "ReactDOM",
  },
};
