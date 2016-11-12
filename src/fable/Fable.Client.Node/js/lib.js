var path = require("path") || require("./path");
var constants = require("./constants");

/**
 * Makes a node-style asynchronous function return an promise.
 * Pass the function and then the rest of arguments.
 * Example: `promisify(fs.remove, "build").then(() => ...)`
*/
function promisify(f) {
    var args = Array.from(arguments).slice(1);
    return new Promise(function (resolve, reject) {
        args.push(function (err, data) {
            if (err) { reject(err); } else { resolve(data); }
        });
        f.apply(this, args);
    });
}
exports.promisify = promisify;

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

/**
 * Checks if the file is an F# project (.fsproj) or script (.fsx)
 * @param {string} filePath The F# project file
 */
function isFSharpProject(filePath) {
    return typeof filePath === "string"
        && constants.FSHARP_PROJECT_EXTENSIONS.indexOf(path.extname(filePath.toLowerCase())) >= 0;
}
exports.isFSharpProject = isFSharpProject;

/**
 * Apparently path.isAbsolute is not very reliable
 * so this uses `path.resolve(x) === x`
 * @param {string} filePath
*/
function isFullPath(filePath) {
    return path.resolve(filePath) === filePath;
}
exports.isFullPath = isFullPath;

/**
 * If path2 is absolute, returns it instead of joining
 * @param {string} path1
 * @param {string} path2
*/
function pathJoin(path1, path2) {
    if (!path2) { return path1 };
    return isFullPath(path2) ? path2 : path.join(path1, path2);
}
exports.pathJoin = pathJoin;

/**
 * Calculates the common parent directory of an array of file paths
 * @param {string[]} filePaths Array of resolved file paths.
*/
function getCommonBaseDir(filePaths) {
    function getCommonPrefix(xs) {
        function f(prefix, xs) {
            if (xs.length === 0) {
                return prefix;
            }
            else {
                var x = xs[0], i = 0;
                while (i < prefix.length && i < x.length && x[i] === prefix[i]) {
                    i = i + 1;
                }
                return f(prefix.slice(0, i), xs.slice(1));
            }
        }
        return xs.length === 0 ? [] : f(xs[0], xs.slice(1));
    }
    var normalized = filePaths.map(function (filePath) {
        return path.dirname(filePath).replace(/\\/g, '/').split('/');
    });
    return getCommonPrefix(normalized).join('/');
}
exports.getCommonBaseDir = getCommonBaseDir;

/**
 * Converts a Babel AST to JS code.
 */
function babelify(babelAst, opts) {
    var babel = require("babel-core");
    // var outDir = pathJoin(opts.workingDir, opts.outDir);

    var babelOpts = {
        babelrc: opts.babelrc || false,
        filename: babelAst.fileName,
        // sourceRoot: outDir,
        presets: opts.babel.presets,
        plugins: opts.babel.plugins,
    };

    var fsCode = null;
    // The F# code is only necessary when generating source maps
    if (opts.sourceMaps && babelAst.originalFileName) {
        try {
            var fs = require("fs");
            fsCode = fs.readFileSync(babelAst.originalFileName);
            babelOpts.sourceMaps = opts.sourceMaps,
            babelOpts.sourceMapTarget = path.basename(babelAst.fileName),
            babelOpts.sourceFileName = path.relative(path.dirname(babelAst.fileName),
            babelAst.originalFileName.replace(/\\/g, '/'));
        }
        catch (err) {
            // Do nothing
        }
    }

    var parsed = babel.transformFromAst(babelAst, fsCode, babelOpts);
    var res = [{
        isEntry: babelAst.isEntry,
        fileName: babelAst.fileName,
        code: parsed.code,
        map: parsed.map
    }];

    // Compile JS includes
    if (Array.isArray(babelAst.jsIncludes))
        babelAst.jsIncludes.forEach(js => {
            parsed = babel.transformFileSync(js.sourcePath, babelOpts);
            res.push({
                fileName: path.join(pathJoin(opts.workingDir, opts.outDir), "js_includes", js.name) + ".js",
                code: parsed.code,
                map: parsed.map
            });
        });

    var timestamp = " at " + (new Date()).toLocaleTimeString();
    for (var i=0; i<res.length; i++) {
        stdoutLog("Compiled " + path.basename(res[i].fileName) + timestamp);
    }

    return res;
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

/** Converts Babel AST to JS code and writes to disc */
function babelifyToFile(babelAst, opts) {
    // Use strict equality so it evals to false when opts.sourceMaps === "inline"
    babelify(babelAst, opts).forEach(parsed =>
        writeFile(parsed.fileName, parsed.code,
            opts.sourceMaps === true ? parsed.map : null));
}
exports.babelifyToFile = babelifyToFile;
