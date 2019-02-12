module.exports = {
    target: "webworker",
    entry: resolve('Worker.fsproj'),
    output: {
        filename: 'index.js',
        path: resolve('../../dist/worker'),
    },
    node: {
        // Mock Node.js modules that Babel require()s but that we don't care about.
        fs: "empty",
        module: "empty",
        net: "empty",
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