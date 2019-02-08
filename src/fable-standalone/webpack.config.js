var path = require("path");

module.exports = {
    entry: resolve('out/Main.js'),
    output: {
        filename: 'fable-standalone.min.js',
        path: resolve('dist'),
        library: 'Fable',
        libraryTarget: 'umd',
        // https://github.com/webpack/webpack/issues/6525
        globalObject: `typeof self !== 'undefined' ? self : this`,
    },
    mode: "production",
};

function resolve(x) {
    return path.join(__dirname, x);
}