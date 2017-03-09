var babel = require("babel-core");
var client = require("./src/client.js");
var babelPlugins = require("./src/babel-plugins.js");

var DEFAULT_PORT = 61225;

function or(option, _default) {
    return option !== void 0 ? option : _default;
}

function join(arg) {
    return Array.isArray(arg) ? arg.join(";") : arg;
}

module.exports = function(buffer) {
    this.cacheable();
    var callback = this.async();
    var opts = this.loaders[0].options || {};
    var port = or(opts.port, DEFAULT_PORT);

    var babelOptions = opts.babel || {};
    babelOptions.plugins = [
        babelPlugins.transformMacroExpressions,
        babelPlugins.removeUnneededNulls,
    ].concat(babelOptions.plugins || []);

    var msg = {
        path: this.resourcePath,
        define: or(opts.define, []),
        plugins: or(opts.plugins, []),
        options: {
            declaration: or(opts.declaration, false),
            typedArrays: or(opts.typedArrays, true),
            clampByteArrays: or(opts.clampByteArrays, false),
        }
    };
    console.log("Fable client sent: " + msg.path)

    client.send(port, JSON.stringify(msg))
        .then(data => {
            var data = JSON.parse(data);
            if (data.error) {
                callback(data.error);
            }
            else {
                console.log("Fable client received: " + msg.path)
                data.infos.forEach(x => console.log(x))
                data.warnings.forEach(x => this.emitWarning(x))
                var transformed = babel.transformFromAst(data, null, babelOptions);
                // TODO: Mark dependencies
                callback(null, transformed.code);
            }
        })
        .catch(err => {
            var msg = err.message + "\nMake sure Fable server is running on port " + port;
            callback(new Error(msg))
        })
};
module.exports.raw = true;
