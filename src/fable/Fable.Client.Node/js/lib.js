var path = require("path") || require("./path");
var constants = require("./constants");

/** Prints a new line with the message on process.stderr */
function stderrLog(tag, err) {
    var prefix = null;
    if (err) {
        prefix = "[" + tag + " ERROR] ";
    }
    else {
        prefix = "[ERROR] ";
        err = tag;
    }

    if (typeof err === "object" && err.message) {
        err = err.message + (err.stack ? "\n" + err.stack : "")
    }
    err = String(err);

    if (typeof process === "object") {
        process.stderr.write(prefix + err + "\n");
    }
    else {
        console.log(err);
    }
}
exports.stderrLog = stderrLog;

/** Prints a new line with the message on process.stdout */
function stdoutLog(s) {
    if (typeof process === "object") {
        process.stdout.write(s + "\n");
    }
    else {
        console.log(s);
    }
}
exports.stdoutLog = stdoutLog;

/** Finish the process according to the environment */
function finish(code, continuation) {
    var err = code === 0 ? null : "FABLE EXIT CODE: " + code;
    if (typeof continuation === "object") {
        if (err && typeof continuation.reject === "function") {
            continuation.reject(err);
            return;
        }
        else if (typeof continuation.resolve === "function") {
            continuation.resolve();
            return;
        }
    }
    if (typeof process === "object") {
        process.exit(code);
    }
    else if (err) {
        throw err;
    }
}
exports.finish = finish;

function splitByWhitespace(str) {
    function stripQuotes(str, start, end) {
        return str[start] === '"' && str[end - 1] === '"'
                ? str.substring(start + 1, end - 1)
                : str.substring(start, end);
    }
    var reg = /\s+(?=([^"]*"[^"]*")*[^"]*$)/g;
    reg.lastIndex = 0;
    var tmp, tmp2, results = [], lastIndex = 0;
    while ((tmp = reg.exec(str)) !== null) {
        results.push(stripQuotes(str, lastIndex, tmp.index));
        lastIndex = tmp.index + tmp[0].length;
    }
    results.push(stripQuotes(str, lastIndex, str.length));
    return results;
}
exports.splitByWhitespace = splitByWhitespace;

function runCommandPrivate(workingDir, command, resolve, reject) {
    var child_process = require('child_process');
    var cmd, args;
    process.stdout.write(workingDir + "> " + command + "\n");
    // If there's no continuation, it means the process will run in parallel (postbuild-once).
    // If we use `cmd /C` on Windows we won't be able to kill the cmd child process later.
    // See http://stackoverflow.com/a/32814686 (unfortutanely the solutions didn't seem to apply here)
    if (process.platform === "win32" && resolve) {
        cmd = "cmd";
        args = splitByWhitespace(command);
        args.splice(0,0,"/C");
    }
    else {
        args = splitByWhitespace(command);
        cmd = args[0];
        args = args.slice(1);
    }
    var proc = child_process.spawn(cmd, args, { cwd: workingDir });
    proc.on('exit', function(code) {
        if (code === 0 && typeof resolve === "function")
            resolve(code);
        else if (code !== 0 && typeof reject === "function")
            reject(code);
    });
    proc.stderr.on('data', function(data) {
        stderrLog(data.toString());
    });
    proc.stdout.on("data", function(data) {
        stdoutLog(data.toString());
    });
    return proc;
}

/** Runs a command and returns a Promise, requires child_process */
function runCommand(workingDir, command) {
    return new Promise(function (resolve, reject) {
        runCommandPrivate(workingDir, command, resolve, reject)
    });
}
exports.runCommand = runCommand;

/** Starts a process to run the command and returns it, requires child_process */
function runCommandInParallel(workingDir, command) {
    return runCommandPrivate(workingDir, command);
}
exports.runCommandInParallel = runCommandInParallel;

/**
 * Returns an array with tuples of plugin paths and config objects (requires 'resolve' package)
 * @param plugins Can be a string, array of tuples (id + config) or an object (key-value pairs)
 * @param basedir Directory from where to resolve the plugins
 * @param prefix Will be attached to plugin names if missing (e.g. 'babel-plugin-')
*/
function resolvePlugins(plugins, basedir, prefix) {
    if (plugins == null) {
        return [];
    }
    else if (typeof plugins === "object") {
        if (!Array.isArray(plugins)) {
            plugins = Object.getOwnPropertyNames(plugins).map(function (k) { return [k, plugins[k]] });
        }
    }
    else {
        plugins = [plugins];
    }

    var resolve = require("resolve");
    return plugins.map(function (plugin) {
        var config = {};
        if (Array.isArray(plugin)) {
            config = plugin[1];
            plugin = plugin[0];
        }
        plugin = prefix && !plugin.startsWith(prefix) ? prefix + plugin : plugin;
        return [resolve.sync(plugin, { basedir: basedir }), config];
    });
}
exports.resolvePlugins = resolvePlugins;

function isFSharpProject(filePath) {
    return typeof filePath === "string"
        && constants.FSHARP_PROJECT_EXTENSIONS.indexOf(path.extname(filePath.toLowerCase())) >= 0;
}
exports.isFSharpProject = isFSharpProject;

/**
 * Apparently path.isAbsolute is not very reliable
 * so this uses `path.resolve(x) === x`
*/
function isFullPath(filePath) {
    return path.resolve(filePath) === filePath;
}
exports.isFullPath = isFullPath;

/** If path2 is absolute, returns it instead of joining */
function pathJoin(path1, path2) {
    return isFullPath(path2) ? path2 : path.join(path1, path2);
}
exports.pathJoin = pathJoin;

/**
 * Converts a Babel AST to JS code. `fsCode` is optional,
 * if `path` is null, Node's "path" module will be used.
 */
function babelify(babelAst, fsCode, opts) {
    var babel = require("babel-core");

    var outDir = pathJoin(opts.workingDir, opts.outDir),
        projDir = path.dirname(pathJoin(opts.workingDir, opts.projFile));

    var targetFile =
        pathJoin(outDir, path.relative(projDir, babelAst.fileName))
            .replace(/\\/g, '/')
            .replace(path.extname(babelAst.fileName), ".js");

    var babelOpts = {
        babelrc: opts.babelrc || false,
        filename: targetFile,
        sourceRoot: outDir,
        presets: opts.babel.presets,
        plugins: opts.babel.plugins,
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
function babelifyToFile(babelAst, opts) {
    var fs = require("fs");

    // The F# code is only necessary when generating source maps
    var fsCode = opts.sourceMaps && babelAst.originalFileName
        ? fs.readFileSync(babelAst.originalFileName)
        : null;

    var parsed = babelify(babelAst, fsCode, opts);
    // Use strict equality so it evals to false when opts.sourceMaps === "inline"
    writeFile(parsed.fileName, parsed.code,
        opts.sourceMaps === true ? parsed.map : null);
}
exports.babelifyToFile = babelifyToFile;
