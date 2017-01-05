import * as fs from "fs";
import * as path from "path";
import * as child_process from "child_process";
import * as babel from "babel-core";
import * as resolve from "resolve";
import * as constants from "./constants";
import { FableOptions, BabelAst, BabelFile, Continuation } from "./types";

/**
 * Makes a node-style asynchronous function return an promise.
 * Pass the function and then the rest of arguments.
 * Example: `promisify(fs.remove, "build").then(() => ...)`
*/
export function promisify(f: Function) {
    var args = Array.from(arguments).slice(1);
    return new Promise(function (resolve, reject) {
        args.push(function (err: Error, data: any) {
            if (err) { reject(err); } else { resolve(data); }
        });
        f.apply(null, args);
    });
}

/** Prints a new line with the message on process.stderr */
export function stderrLog(tag: string | any, err?: any) {
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

/** Prints a new line with the message on process.stdout */
export function stdoutLog(s: string) {
    if (typeof process === "object") {
        process.stdout.write(s + "\n");
    }
    else {
        console.log(s);
    }
}

/** Finish the process according to the environment */
export function finish(code: number, continuation?: Continuation) {
    var err = code === 0 ? null : "FABLE EXIT CODE: " + code;
    if (typeof continuation === "object") {
        if (err && typeof continuation.reject === "function") {
            continuation.reject(err);
            return;
        }
        else if (typeof continuation.resolve === "function") {
            continuation.resolve(null);
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

export function splitByWhitespace(str: string) {
    function stripQuotes(str: string, start: number, end: number) {
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

function runCommandPrivate(workingDir: string, command: string, continuation?: Continuation) {
    var cmd, args;
    process.stdout.write(workingDir + "> " + command + "\n");
    // If there's no continuation, it means the process will run in parallel (postbuild-once).
    // If we use `cmd /C` on Windows we won't be able to kill the cmd child process later.
    // See http://stackoverflow.com/a/32814686 (unfortutanely the solutions didn't seem to apply here)
    if (process.platform === "win32" && continuation) {
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
    proc.on('exit', function(code: number) {
        if (continuation) {
            code === 0 ? continuation.resolve(code) : continuation.reject(code);
        }
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
export function runCommand(workingDir: string, command: string) {
    return new Promise(function (resolve, reject) {
        runCommandPrivate(workingDir, command, { resolve, reject })
    });
}

/** Starts a process to run the command and returns it, requires child_process */
export function runCommandInParallel(workingDir: string, command: string) {
    return runCommandPrivate(workingDir, command);
}

/**
 * Returns an array with tuples of plugin paths and config objects (requires 'resolve' package)
 * @param plugins Can be a string, array of tuples (id + config) or an object (key-value pairs)
 * @param basedir Directory from where to resolve the plugins
 * @param prefix Will be attached to plugin names if missing (e.g. 'babel-plugin-')
*/
export function resolvePlugins(plugins: any, basedir: string, prefix: string) {
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

    return plugins.map(function (plugin: any) {
        var config = {};
        if (Array.isArray(plugin)) {
            config = plugin[1];
            plugin = plugin[0];
        }
        plugin = prefix && !plugin.startsWith(prefix) ? prefix + plugin : plugin;
        return [resolve.sync(plugin, { basedir: basedir }), config];
    });
}

/**
 * Checks if the file is an F# project (.fsproj) or script (.fsx)
 */
export function isFSharpProject(filePath: string) {
    return typeof filePath === "string"
        && constants.FSHARP_PROJECT_EXTENSIONS.indexOf(path.extname(filePath).toLowerCase()) >= 0;
}

/**
 * Checks if the file is an F# module (.fs), script (.fsx) or project (.fsproj)
 */
export function isFSharpFile(filePath: string) {
    return typeof filePath === "string"
        && constants.FSHARP_FILE_EXTENSIONS.indexOf(path.extname(filePath).toLowerCase()) >= 0;
}

/**
 * Apparently path.isAbsolute is not very reliable
 * so this uses `path.resolve(x) === x`
*/
export function isFullPath(filePath: string) {
    return path.resolve(filePath) === filePath;
}

/**
 * If path2 is absolute, returns it instead of joining
*/
export function pathJoin(path1: string, path2: string) {
    if (!path2) { return path1 };
    return isFullPath(path2) ? path2 : path.join(path1, path2);
}

/**
 * Calculates the common parent directory of an array of file paths
 * @param {string[]} filePaths Array of resolved file paths.
*/
export function getCommonBaseDir(filePaths: string[]) {
    function getCommonPrefix(xs: string[][]): string[] {
        function f(prefix: string[], xs: string[][]): string[] {
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
    var normalized = filePaths.map(filePath => path.dirname(filePath).replace(/\\/g, '/').split('/'));
    return getCommonPrefix(normalized).join('/');
}

/**
 * Converts a Babel AST to JS code.
 */
export function babelify(babelAst: BabelAst, opts: FableOptions): BabelFile[] {
    var outDir = pathJoin(opts.workingDir, opts.outDir);

    var babelOpts: babel.TransformOptions = {
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
            fsCode = fs.readFileSync(babelAst.originalFileName).toString();
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
                isEntry: false,
                fileName: pathJoin(outDir, "js_includes/" + js.name) + ".js",
                code: parsed.code,
                map: parsed.map
            });
        });

    var timestamp = " at " + (new Date()).toLocaleTimeString();
    for (var i=0; i<res.length; i++) {
        stdoutLog("Compiled " + path.relative(outDir, res[i].fileName) + timestamp);
    }

    return res;
}

/** Create directory if it doesn't exist */
export function ensureDirExists(dir: string, cont?: ()=>void) {
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

export function writeFile(fileName: string, code: string, map: any) {
    ensureDirExists(path.dirname(fileName));
    fs.writeFileSync(fileName, code);
    if (map) {
        fs.appendFileSync(fileName, "\n//# sourceMappingURL=" + path.basename(fileName) + ".map");
        fs.writeFileSync(fileName + ".map", JSON.stringify(map));
    }
}

/** Converts Babel AST to JS code and writes to disc */
export function babelifyToFile(babelAst: BabelAst, opts: FableOptions) {
    // Use strict equality so it evals to false when opts.sourceMaps === "inline"
    babelify(babelAst, opts).forEach(parsed =>
        writeFile(parsed.fileName, parsed.code,
            opts.sourceMaps === true ? parsed.map : null));
}
