var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
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

function transformBabelAst(babelAst, babelOptions, sourcePath, sourceCodeBuffer) {
    var fsCode = null;
    if (this.sourceMap && sourcePath && sourceCodeBuffer) {
        fsCode = sourceCodeBuffer.toString();
        babelOptions.sourceMaps = true;
        babelOptions.sourceFileName = path.relative(process.cwd(), sourcePath.replace(/\\/g, '/'));
    }
    return babel.transformFromAst(babelAst, fsCode, babelOptions);
}

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

    // console.log("fable: Requested " + msg.path)
    // console.log("Full message: " + JSON.stringify(msg))

    client.send(port, JSON.stringify(msg))
        .then(data => {
            var data = JSON.parse(data);
            if (data.error) {
                callback(new Error(data.error));
            }
            else {
                try {
                    var babelParsed;
                    if (data.cache) {
                        var babelCache = data.cache + ".babel";
                        if (fs.existsSync(babelCache)) {
                            // console.log("Babel cache found: " + msg.path)
                            try {
                                babelParsed = JSON.parse(fs.readFileSync(babelCache, "utf8").toString());
                            }
                            catch (err) {
                                fs.unlinkSync(babelCache);
                                throw err;
                            }
                        }
                        else {
                            // console.log("No Babel cache: " + msg.path)
                            var fableAst = JSON.parse(fs.readFileSync(data.cache, "utf8").toString());
                            babelParsed = transformBabelAst(fableAst, babelOptions, data.fileName, buffer);
                            fs.writeFileSync(babelCache, JSON.stringify(babelParsed), {encoding: "utf8"});
                        }
                    }
                    else {
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
                        var fsCode = null;
                        if (this.sourceMap) {
                            fsCode = buffer.toString();
                            babelOptions.sourceMaps = true;
                            babelOptions.sourceFileName = path.relative(process.cwd(), data.fileName.replace(/\\/g, '/'));
                        }
                        babelParsed = transformBabelAst(data, babelOptions, data.fileName, buffer);
                    }
                    console.log("fable: Compiled " + path.basename(msg.path));
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
module.exports.raw = true;