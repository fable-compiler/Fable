var path = require("path");
var babel = require("babel-core");
var client = require("fable-utils/client");
var babelPlugins = require("fable-utils/babel-plugins");

var DEFAULT_PORT = 61225;
var fableCoreVersion = null;

function or(option, _default) {
    return option !== void 0 ? option : _default;
}

function join(arg) {
    return Array.isArray(arg) ? arg.join(";") : arg;
}

function ensureArray(obj) {
    return Array.isArray(obj) ? obj : (obj != null ? [obj] : []);
}

var customPlugins = [
    babelPlugins.getRemoveUnneededNulls(),
    babelPlugins.getTransformMacroExpressions(babel.template)
];

module.exports = function(buffer) {
    this.cacheable();
    var callback = this.async();
    var opts = this.loaders[0].options || {};
    var port = or(opts.port, DEFAULT_PORT);

    var babelOptions = opts.babel || {};
    babelOptions.plugins = customPlugins.concat(babelOptions.plugins || []);

    var msg = {
        path: this.resourcePath,
        define: ensureArray(or(opts.define, [])),
        plugins: ensureArray(or(opts.plugins, [])),
        fableCore: or(opts.fableCore, null),
        declaration: or(opts.declaration, false),
        typedArrays: or(opts.typedArrays, true),
        clampByteArrays: or(opts.clampByteArrays, false),
        extra: opts.extra
    };

    if (opts.fableCore == null) {
        if (fableCoreVersion == null) {
            fableCoreVersion = require("fable-core/package.json").version;
        }
        msg.fableCore = path.join(__dirname, "../fable-core");
        msg.fableCoreVersion = fableCoreVersion;
    }
    else {
        msg.fableCoreVersion = "*";
    }

    console.log("Fable loader sent: " + msg.path)
    // console.log("Full message: " + JSON.stringify(msg))

    client.send(port, JSON.stringify(msg))
        .then(data => {
            var data = JSON.parse(data);
            if (data.error) {
                callback(new Error(data.error));
            }
            else {
                console.log("Fable loader received: " + msg.path);
                if (typeof data.logs === "object") {
                    Object.keys(data.logs).forEach(key => {
                        // TODO: Fail if there's one or more error logs?
                        // That would prevent compilation of other files
                        ensureArray(data.logs[key]).forEach(msg => {
                            switch (key)  {
                                case "error":
                                    this.emitError(new Error(msg));
                                    break;
                                case "warning":
                                    this.emitWarning(new Error(msg));
                                    break;
                                default:
                                    console.log(msg)
                            }
                        });
                    })
                }
                try {
                    var fsCode = null;
                    if (this.sourceMap) {
                        fsCode = buffer.toString();
                        babelOptions.sourceMaps = true;
                        babelOptions.sourceFileName = path.relative(process.cwd(), data.fileName.replace(/\\/g, '/'));
                    }
                    var transformed = babel.transformFromAst(data, fsCode, babelOptions);
                    callback(null, transformed.code, transformed.map);
                }
                catch (err) {
                    callback(err)
                }
            }
        })
        .catch(err => {
            var msg = err.message + "\nMake sure Fable server is running on port " + port;
            callback(new Error(msg))
        })
};
module.exports.raw = true;
