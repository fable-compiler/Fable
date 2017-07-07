/// @ts-check

var path = require("path");
var babel = require("babel-core");
var cache = require("fable-utils/cache");
var client = require("fable-utils/client");
var babelPlugins = require("fable-utils/babel-plugins");

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
    babelPlugins.getRemoveUnneededNulls(),
    babelPlugins.getTransformMacroExpressions(babel.template)
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

    cache.tryLoadCache(opts.extra, msg.path).then(cache => {
        if (cache != null) {
            console.log("fable: Cached " + path.basename(msg.path));
            return cache;
        }
        else {
            return client.send(port, JSON.stringify(msg));
        }
    })
    .then(r => {
        if (r instanceof cache.CachedFile) {
            callback(null, r.code, r.map);
            return;
        }

        var data = JSON.parse(r);
        if (data.error) {
            callback(new Error(data.error));
        }
        else {
            try {
                ensureArray(data.dependencies).forEach(path => {
                    this.addDependency(path)
                });
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
                var sourceMapOpts = this.sourceMap ? {
                    path: data.fileName,
                    buffer: buffer
                } : null;
                var babelParsed = transformBabelAst(data, babelOptions, sourceMapOpts);
                console.log("fable: Compiled " + path.relative(process.cwd(), msg.path));
                cache.trySaveCache(opts.extra, data.fileName, babelParsed);
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