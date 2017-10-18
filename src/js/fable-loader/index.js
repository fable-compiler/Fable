/// @ts-check

var path = require("path");
var babel = require("babel-core");
var fableUtils = require ("fable-utils");

var DEFAULT_PORT =
    process.env.FABLE_SERVER_PORT != null
    ? parseInt(process.env.FABLE_SERVER_PORT, 10)
    : 61225;

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
    fableUtils.babelPlugins.getRemoveUnneededNulls(),
    fableUtils.babelPlugins.getTransformMacroExpressions(babel.template)
];

function transformBabelAst(babelAst, babelOptions, sourceMapOptions) {
    var fsCode = null;
    if (sourceMapOptions != null) {
        fsCode = sourceMapOptions.buffer.toString();
        babelOptions.sourceMaps = true;
        babelOptions.sourceFileName = path.relative(process.cwd(), sourceMapOptions.path.replace(/\\/g, '/'));
    }
    return babel.transformFromAst(babelAst, fsCode, babelOptions);
}

var Loader = function(buffer) {
    var callback = this.async();
    var opts = this.loaders[0].options || {};
    try {
        fableUtils.validateFableOptions(opts);
    }
    catch (err) {
        callback(err);
        return;
    }

    var port = or(opts.port, DEFAULT_PORT);
    var babelOptions = opts.babel || {};
    babelOptions.plugins = customPlugins.concat(babelOptions.plugins || []);

    var msg = {
        path: this.resourcePath,
        define: ensureArray(or(opts.define, [])),
        plugins: ensureArray(or(opts.plugins, [])),
        fableCore: or(opts.fableCore, null),
        typedArrays: or(opts.typedArrays, true),
        clampByteArrays: or(opts.clampByteArrays, false),
        extra: opts.extra
    };

    fableUtils.client.send(port, JSON.stringify(msg)).then(r => {
        var data = JSON.parse(r);
        if (data.error) {
            callback(new Error(data.error));
        }
        else {
            try {
                // Fable now returns all file projects as `.dependencies` for .fsproj
                // If we add them to Webpack, .fsproj will be recompiled every time
                if (!msg.path.endsWith(".fsproj")) {
                    ensureArray(data.dependencies).forEach(path => {
                        this.addDependency(path)
                    });
                }
                if (typeof data.logs === "object") {
                    var isErrored = false;
                    Object.keys(data.logs).forEach(key => {
                        // TODO: Fail if there's one or more error logs?
                        // That would prevent compilation of other files
                        ensureArray(data.logs[key]).forEach(msg => {
                            switch (key)  {
                                case "error":
                                    isErrored = true;
                                    this.emitError(new Error(msg));
                                    break;
                                case "warning":
                                    this.emitWarning(new Error(msg));
                                    break;
                                default:
                                    console.log(msg)
                            }
                        });
                      });
                      this.cacheable(!isErrored);
                }
                var sourceMapOpts = this.sourceMap ? {
                    path: data.fileName,
                    buffer: buffer
                } : null;
                var babelParsed = transformBabelAst(data, babelOptions, sourceMapOpts);
                console.log("fable: Compiled " + path.relative(process.cwd(), msg.path));
                callback(null, babelParsed.code, babelParsed.map);
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

Loader.raw = true;
module.exports = Loader;