var path = require("path");
var MinifyPlugin = require("terser-webpack-plugin");

module.exports = {
    entry: resolve('out/Main.js'),
    output: {
        filename: 'bundle.min.js',
        path: resolve('bundle'),
        library: 'Fable'
    },
    mode: "production",
    optimization: {
        minimizer: [new MinifyPlugin()]
    }
};

function resolve(x) {
    return path.join(__dirname, x);
}