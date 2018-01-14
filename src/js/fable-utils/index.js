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

/**
 * Validates Fable options, at the moment it just checks
 * if Babel options have been resolved
 */
exports.validateFableOptions = function(opts) {
    var path = require("path");
    function validateArray(babelOpts, category) {
        if (Array.isArray(babelOpts[category])) {
            babelOpts[category].forEach(function(el) {
                var elPath = Array.isArray(el) ? el[0] : el;
                if (typeof elPath === "string" && !path.isAbsolute(elPath)) {
                    throw new Error(
                        `"${elPath}" in babel.${category} is not resolved to an absolute path. ` +
                        `Please use \`require("fable-utils").resolveBabelOptions(babelOptions)\` ` +
                        `(also when passing the options to other plugins like babel-loader). ` +
                        `Check https://goo.gl/dsBqWA for an example.`
                    );
                }
            });
        }
    }
    if (typeof opts.babel === "object") {
        validateArray(opts.babel, "presets");
        validateArray(opts.babel, "plugins");
    }
}