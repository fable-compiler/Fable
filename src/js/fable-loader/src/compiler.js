var compiler = require("../../fable-compiler-dotnet");

var cache = null;

module.exports = function getCompiler(webpack, args) {
    if (cache == null) {
        cache = compiler.default(args);
        if (!webpack.watchMode) {
            webpack.hooks.done.tap("fable-loader", function() {
                cache.close();
            });
        }
    }
    return cache;
}
