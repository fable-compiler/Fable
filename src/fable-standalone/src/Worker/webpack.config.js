module.exports = {
    entry: resolve('Worker.fsproj'),
    output: {
        filename: 'worker.min.js',
        path: resolve('../../dist'),
        // https://github.com/webpack/webpack/issues/6525
        globalObject: `typeof self !== 'undefined' ? self : this`,
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