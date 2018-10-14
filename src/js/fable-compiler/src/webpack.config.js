var path = require("path");
var MinifyPlugin = require("terser-webpack-plugin");

function resolve(x) {
    return path.join(__dirname, x);
}

module.exports = {
    target: "node",
    entry: resolve('../out/app.js'),
    output: {
        filename: 'index.js',
        path: resolve('../dist'),
        // library: 'Fable'
    },
    mode: "production",
    optimization: {
        minimizer: [new MinifyPlugin()]
    },
};
