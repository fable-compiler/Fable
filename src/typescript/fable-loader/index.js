var os = require("os")
var fs = require("fs");
var path = require("path");
var crypto = require('crypto');
var babel = require("babel-core");
var msgpack = require("msgpack-lite");
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

function getCachePath(fileName) {
    var hash = crypto.createHash('md5').update(fileName).digest('hex');
    return path.join(os.tmpdir(), "fable", hash);
}

function tryLoadCache(opts, fileName) {
    if (!opts.extra || !opts.extra.useCache || path.extname(fileName) !== ".fs") {
        return Promise.resolve(null);
    }

    return new Promise(resolve => {
        var cachePath = getCachePath(fileName);
        if (fs.existsSync(cachePath)) {
            var sourcemtime = fs.statSync(fileName).mtime;
            var cachemtime = fs.statSync(cachePath).mtime;
            if (sourcemtime < cachemtime) {
                var readStream = fs.createReadStream(cachePath);
                var decodeStream = msgpack.createDecodeStream();
                readStream.pipe(decodeStream).on("data", data => { resolve(data) });
                return;
                // return JSON.parse(fs.readFileSync(cachePath, "utf8").toString());
            }
        }
        resolve(null);
    });
}

function trySaveCache(opts, fileName, data) {
    if (opts.extra && opts.extra.useCache && opts.extra.useCache !== "readonly") {
        // fs.writeFileSync(getCachePath(data.fileName), JSON.stringify(babelParsed), {encoding: "utf8"});
        var writeStream = fs.createWriteStream(getCachePath(fileName));
        var encodeStream = msgpack.createEncodeStream();
        encodeStream.pipe(data);
        encodeStream.write(babelParsed);
        encodeStream.end();
    }
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


    tryLoadCache(opts, msg.path).then(cache => {
        if (cache != null) {
            console.log("fable: Cached " + path.basename(msg.path));
            callback(null, cache.code, cache.map);
            return Promise.resolve(null);
        }
        else {
            return client.send(port, JSON.stringify(msg));
        }
    })
    .then(data => {
        if (data == null) {
            return;
        }

        data = JSON.parse(data);
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
                var babelParsed = transformBabelAst(data, babelOptions, data.fileName, buffer);
                console.log("fable: Compiled " + path.basename(msg.path));
                trySaveCache(opts, data.fileName, babelParsed);
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