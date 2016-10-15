var path = require("path") || require("./path");

/** Prints a new line with the message on process.stderr */
function stderrLog(s) {
    if (typeof process === "object") {
        process.stderr.write(s + "\n");
    }
    else {
        console.log(s);
    }
}
exports.stderrLog = stderrLog;

/** Prints a new line with the message on process.stdout */
function stdoutLog(s) {
    if (typeof process === "object") {
        process.stdout.write(s + "\n")
    }
    else {
        console.log(s);
    }
}
exports.stdoutLog = stdoutLog;

/** Finish the process according to the environment */
function finish(code, opts, resolve, reject) {
    var err = code === 0 ? null : "FABLE EXIT CODE: " + code;
    if (typeof resolve === "function") {
        if (err && typeof reject === "function")
            reject(err);
        else
            resolve();
    }
    else {
        if (typeof process === "object") {
            process.exit(code);
        }
        else if (err) {
            throw err;
        }
    }
}
exports.finish = finish;

/**
 * Converts a Babel AST to JS code. `fsCode` is optional,
 * if `path` is null, Node's "path" module will be used.
 */
function babelify(babelAst, fsCode, babelOpts, opts) {
    var babel = require("babel-core");

    var outDir = path.resolve(path.join(opts.workingDir, opts.outDir)),
        projDir = path.dirname(path.resolve(path.join(opts.workingDir, opts.projFile)));

    var targetFile =
        path.join(outDir, path.relative(projDir, path.resolve(babelAst.fileName)))
            .replace(/\\/g, '/')
            .replace(path.extname(babelAst.fileName), ".js");

    babelOpts = {
        babelrc: opts.babelrc || false,
        filename: targetFile,
        sourceRoot: outDir,
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
        isEntry: babelAst.isEntry,
        fileName: targetFile,
        code: parsed.code,
        map: parsed.map
    };    
}
exports.babelify = babelify;

/** Create directory if it doesn't exist, requires 'fs' module */
function ensureDirExists(dir, cont) {
    var fs = require("fs");
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
exports.ensureDirExists = ensureDirExists;

function writeFile(fileName, code, map) {
    var fs = require("fs");
    ensureDirExists(path.dirname(fileName));
    fs.writeFileSync(fileName, code);
    if (map) {
        fs.appendFileSync(fileName, "\n//# sourceMappingURL=" + path.basename(fileName) + ".map");
        fs.writeFileSync(fileName + ".map", JSON.stringify(map));
    }
}
exports.writeFile = writeFile;

/** Converts a Babel AST to JS code and writes to disc, requires 'fs' module */
function babelifyToFile(babelAst, babelOpts, opts) {
    var fs = require("fs");
    
    // The F# code is only necessary when generating source maps
    var fsCode = opts.sourceMaps && babelAst.originalFileName
        ? fs.readFileSync(babelAst.originalFileName)
        : null;

    var parsed = babelify(babelAst, fsCode, babelOpts, opts);
    // Use strict equality so it evals to false when opts.sourceMaps === "inline"
    writeFile(parsed.fileName, parsed.code,
        opts.sourceMaps === true ? parsed.map : null);
}
exports.babelifyToFile = babelifyToFile;
