module.exports = {
  entry: './src/React.TodoMVC.fsproj',
  mode: "production",
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
