var path = require("path");
var MinifyPlugin = require("terser-webpack-plugin");

module.exports = {
    entry: resolve('out/Main.js'),
    output: {
        filename: 'bundle.min.js',
        path: resolve('bundle'),
        library: 'Fable',
        libraryTarget: 'umd',
        // https://github.com/webpack/webpack/issues/6525
        globalObject: `typeof self !== 'undefined' ? self : this`,
    },
    mode: "production",
    optimization: {
        minimizer: [new MinifyPlugin()]
    }
};

function resolve(x) {
    return path.join(__dirname, x);
}