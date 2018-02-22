/// @ts-check

exports.client = require("./client");
exports.babelPlugins = require("./babel-plugins");

/**
 * Resolves location of Babel presets and plugins
 * to avoid problems when compiling files outside
 * the project folder
 */
exports.resolveBabelOptions = function(opts) {
    function resolve(prefix, item) {
        return require.resolve(item.startsWith(prefix) ? item : prefix + item);
    }
    function resolveArray(prefix, ar) {
        return ar.map(item => Array.isArray(item)
            ? [resolve(prefix, item[0]), item[1]]
            : resolve(prefix, item));
    }
    var newOpts = Object.assign({}, opts);
    if (Array.isArray(opts.presets)) {
        newOpts.presets = resolveArray("babel-preset-", opts.presets);
    }
    if (Array.isArray(opts.plugins)) {
        newOpts.plugins = resolveArray("babel-plugin-", opts.plugins);
    }
    return newOpts;
}
