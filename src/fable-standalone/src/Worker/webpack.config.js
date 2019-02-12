module.exports = {
    target: "webworker",
    entry: resolve('Worker.fsproj'),
    output: {
        filename: 'index.js',
        path: resolve('../../dist/worker'),
    },
    mode: "production",
    module: {
        rules: [
          {
            test: /\.fs(x|proj)?$/,
            use: {
                loader: "fable-loader",
                options: {
                    cli: { path: resolve("../../../Fable.Cli") }
                }
            }
          }
        ]
    }
};

function resolve(x) {
    return require("path").join(__dirname, x);
}