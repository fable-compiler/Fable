/// @ts-check

var DEFAULT_COMPILER = "fable-compiler"
// var DEFAULT_COMPILER = require("../fable-compiler"); // testing

var path = require("path");
var babel = require("@babel/core");
var babelPlugins = require("fable-babel-plugins");

function or(option, _default) {
    return option !== void 0 ? option : _default;
}

function ensureArray(obj) {
    return Array.isArray(obj) ? obj : (obj != null ? [obj] : []);
}

var customPlugins = [
    babelPlugins.getRemoveUnneededNulls(),
    babelPlugins.getTransformMacroExpressions(babel.template)
];

var compilerCache = null;

function log(opts, msg) {
    if (!opts.silent) {
        console.log(msg);
    }
}

function getTcpPort(opts) {
    if (opts.port != null) {
        return opts.port;
    } else if (process.env.FABLE_SERVER_PORT != null) {
        return parseInt(process.env.FABLE_SERVER_PORT, 10);
    } else {
        return null;
    }
}

function getCompiler(webpack, args, compiler) {
    if (compilerCache == null) {
        var fable = require(compiler);
        compilerCache = fable.default(args);
        if (!webpack.watchMode) {
            webpack.hooks.done.tap("fable-loader", function() {
                compilerCache.close();
            });
        }
    }
    return compilerCache;
}

function transformBabelAst(babelAst, babelOptions, sourceMapOptions, callback) {
    var fsCode = null;
    if (sourceMapOptions != null) {
        fsCode = sourceMapOptions.buffer.toString();
        babelOptions.sourceMaps = true;
        var fileName = sourceMapOptions.path.replace(/\\/g, '/');
        babelOptions.filename = fileName;
        babelOptions.sourceFileName = path.relative(process.cwd(), fileName);
    }
    babel.transformFromAst(babelAst, fsCode, babelOptions, callback);
}

var Loader = function(buffer) {
    var callback = this.async();

    var opts = this.loaders[0].options || {};
    opts.cli = opts.cli || {};
    opts.cli.silent = opts.silent;
    var babelOptions = opts.babel || {};
    babelOptions.plugins = customPlugins.concat(babelOptions.plugins || []);

    var define = ensureArray(or(opts.define, []));
    try {
        if (this._compiler.options.mode === "development" && define.indexOf("DEBUG") === -1) {
            define.push("DEBUG");
        }
    } catch (er) {}

    var msg = {
        path: this.resourcePath,
        rootDir: process.cwd(),
        define: define,
        typedArrays: or(opts.typedArrays, true),
        clampByteArrays: or(opts.clampByteArrays, false),
        extra: opts.extra || {}
    };

    var port = getTcpPort(opts);
    var command = port != null
        ? require("./net-client").send("127.0.0.1", port, JSON.stringify(msg)).then(json => JSON.parse(json))
        : getCompiler(this._compiler, opts.cli, opts.compiler || DEFAULT_COMPILER).send(msg);

    command.then(data => {
        if (data.error) {
            callback(new Error(data.error));
        }
        else {
            try {
                if (!msg.path.endsWith(".fsproj")) {
                    ensureArray(data.dependencies).forEach(p => {
                        // Fable normalizes path separator to '/' which causes issues in Windows
                        // Use `path.resolve` to restore the separator to the system default
                        this.addDependency(path.resolve(p));
                    });
                }
                if (typeof data.logs === "object") {
                    var isErrored = false;
                    Object.keys(data.logs).forEach(key => {
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
                                    log(opts, msg)
                            }
                        });
                      });
                      this.cacheable(!isErrored);
                }
                var sourceMapOpts = this.sourceMap ? {
                    path: data.fileName,
                    buffer: buffer
                } : null;
                transformBabelAst(data, babelOptions, sourceMapOpts, function (err, babelParsed) {
                    if (err) {
                        callback(err);
                    } else {
                        log(opts, "fable: Compiled " + path.relative(process.cwd(), msg.path));
                        callback(null, babelParsed.code, babelParsed.map);
                    }
                });
            }
            catch (err) {
                callback(err)
            }
        }
    })
    .catch(err => {
        callback(new Error(err.message))
    })
};

Loader.raw = true;
module.exports = Loader;