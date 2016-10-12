/** Prints a new line with the message on process.stderr */
exports.stderrLog = function stderrLog(s) {
    process.stderr.write(s + "\n")
}

/** Prints a new line with the message on process.stdout */
exports.stdoutLog = function stdoutLog(s) {
    process.stdout.write(s + "\n")
}

/**
 * Converts a Babel AST to JS code. `fsCode` is optional,
 * if `path` is null, Node's "path" module will be used.
 */
function babelify(babelAst, fsCode, babelOpts, opts, path) {
    var babel = require("babel-core");
    // For the browser we could use a polyfill
    // like https://github.com/jinder/path or path-browserify    
    path = path || require("path");

    var projDir = path.dirname(path.resolve(path.join(opts.cfgDir, opts.projFile)));
    var targetFile = path.join(path.resolve(opts.outDir), path.relative(projDir, path.resolve(babelAst.fileName)))
                         .replace(/\\/g, '/')
                         .replace(path.extname(babelAst.fileName), ".js");

    babelOpts = {
        babelrc: opts.babelrc || false,
        filename: targetFile,
        sourceRoot: path.resolve(opts.outDir),
        presets: babelOpts.babelPresets,
        plugins: babelOpts.babelPlugins,
    };

    if (opts.sourceMaps && babelAst.originalFileName) {
        babelOpts.sourceMaps = opts.sourceMaps,
        babelOpts.sourceMapTarget = path.basename(targetFile),
        babelOpts.sourceFileName = path.relative(path.dirname(targetFile),
            babelAst.originalFileName).replace(/\\/g, '/')
    }

    var parsed = babel.transformFromAst(babelAst, fsCode, babelOpts);
    return {
        targetFile: targetFile,
        code: parsed.code,
        map: parsed.map
    };    
}
exports.babelify = babelify;

/**
 * Converts a Babel AST to JS code and writes to disc.
 */
function babelifyToFile(babelAst, babelOpts, opts) {
    var fs = require("fs"), path = require("path");

    function ensureDirExists(dir, cont) {
        if (fs.existsSync(dir)) {
            if (typeof cont === "function") { cont(); }
        }
        else {
            ensureDirExists(path.dirname(dir), function() {
                if (!fs.existsSync(dir)) { fs.mkdirSync(dir); }
                if (typeof cont === "function") { cont(); }
            })
        }
    }
    
    // The F# code is only necessary when generating source maps
    var fsCode = opts.sourceMaps && babelAst.originalFileName
        ? fs.readFileSync(babelAst.originalFileName)
        : null;

    var parsed = babelify(babelAst, fsCode, babelOpts, opts);
    ensureDirExists(path.dirname(parsed.targetFile));
    fs.writeFileSync(parsed.targetFile, parsed.code);

    // Use strict equality so it evals to false when opts.sourceMaps === "inline"
    if (opts.sourceMaps === true && babelAst.originalFileName) {
        fs.appendFileSync(parsed.targetFile, "\n//# sourceMappingURL=" + path.basename(parsed.targetFile)+".map");
        fs.writeFileSync(targetFile + ".map", JSON.stringify(parsed.map));
    }
}
exports.babelifyToFile = babelifyToFile;
