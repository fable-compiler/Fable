/** Prints a new line with the message on process.stderr */
exports.stderrLog = function stderrLog(s) {
    process.stderr.write(s + "\n")
}

/** Prints a new line with the message on process.stdout */
exports.stdoutLog = function stdoutLog(s) {
    process.stdout.write(s + "\n")
}

exports.babelifyToFile = function babelifyToFile(babelAst, babelOpts, opts) {
    var fs = require("fs"),
        path = require("path"),
        babel = require("babel-core");

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
    
    var projDir = path.dirname(path.resolve(path.join(opts.cfgDir, opts.projFile)));
    var targetFile = path.join(path.resolve(opts.outDir), path.relative(projDir, path.resolve(babelAst.fileName)))
                         .replace(/\\/g, '/')
                         .replace(path.extname(babelAst.fileName), ".js");
    var fsCode = null,
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
        // The F# code is only necessary when generating source maps
        fsCode = fs.readFileSync(babelAst.originalFileName);
    }

    var parsed = babel.transformFromAst(babelAst, fsCode, babelOpts);
    ensureDirExists(path.dirname(targetFile));
    fs.writeFileSync(targetFile, parsed.code);

    // Use strict equality so it evals to false when opts.sourceMaps === "inline"
    if (opts.sourceMaps === true && babelAst.originalFileName) {
        fs.appendFileSync(targetFile, "\n//# sourceMappingURL=" + path.basename(targetFile)+".map");
        fs.writeFileSync(targetFile + ".map", JSON.stringify(parsed.map));
    }
}
