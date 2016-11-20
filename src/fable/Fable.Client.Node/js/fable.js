var fableLib = require("./lib");
var constants = require("./constants");
var customPlugins = require("./babelPlugins");

// In the browser this should be set to a falsy value
var fs = require("fs");
var path = require("path") || require("./path");

// Don't use default values as they would block options from fableconfig.json
var optionDefinitions = [
  { name: 'projFile', defaultOption: true, multiple: true, description: "The F# project (.fsproj) or script (.fsx) to compile." },
  { name: 'outDir', alias: 'o', description: "Where to put compiled JS files. Defaults to project directory." },
  { name: 'module', alias: 'm', description: "Specify module code generation: `commonjs` (default), `umd`, `amd` or `es2015`." },
  { name: 'sourceMaps', alias: 's', description: "Generate source maps: `false` (default), `true` or `inline`." },
  { name: 'watch', alias: 'w', multiple: 'true', description: "Recompile project much faster on file modifications." },
  { name: 'ecma', description: "Specify ECMAScript target version: `es5` (default) or `es2015`." },
  { name: 'rollup', description: "Bundle files and dependencies with Rollup." },
  { name: 'refs', multiple: true, description: "Alternative location for compiled JS files of referenced libraries (e.g. `Fable.Core=fable-core/umd`)." },
  { name: 'symbols', multiple: true, description: "F# symbols for conditional compilation, like `DEBUG`." },
  { name: 'plugins', multiple: true, description: "Paths to Fable plugins." },
  { name: 'babelPlugins', multiple: true, description: "Additional Babel plugins (without `babel-plugin-` prefix). Must be installed in the project directory." },
  { name: 'loose', type: Boolean, description: "Enable loose transformations for babel-preset-es2015 plugins." },
  { name: 'babelrc', type: Boolean, description: "Use a `.babelrc` file for Babel configuration (invalidates other Babel related options)." },
  { name: 'dll', type: Boolean, description: "Generate a `dll` assembly." },
  { name: 'noTypedArrays', type: Boolean, description: "Don't compile numeric arrays as JS typed arrays." },
  { name: 'clamp', type: Boolean, description: "Compile unsigned byte arrays as Uint8ClampedArray." },
  { name: 'verbose', type: Boolean, description: "Print more information about the compilation process." },
  { name: 'target', alias: 't', description: "Use options from a specific target in `fableconfig.json`." },
  { name: 'debug', alias: 'd', description: "Shortcut for `--target debug`." },
  { name: 'production', alias: 'p', description: "Shortcut for `--target production`." },
  { name: 'declaration', type: Boolean, description: "[EXPERIMENTAL] Generates corresponding ‘.d.ts’ file." },
  { name: 'extra', multiple: true, description: "Custom options for plugins in `Key=Value` format." },
  { name: 'coreLib', description: "[DEPRECATED] Use `--refs Fable.Core=fable-core/umd` instead." },
  { name: 'help', alias: 'h', description: "Display usage guide." }
];

function getAppDescription() {
    return [{ header: 'Fable ' + constants.PKG_VERSION, content: 'F# to JavaScript compiler' },
            { header: 'Options', optionList: optionDefinitions },
            { content: 'All arguments can be defined in a fableconfig.json file' }];
}

/** Processes a JSON received from .NET process. If it's a Babel AST it will be compiled. */
function processJson(json, opts, continuation) {;
    try {
        var babelAst;
        try {
            babelAst = JSON.parse(json);
        }
        catch (_err) {
            return; // If stdout is not in JSON format, just ignore
        }
        if (babelAst.type == "LOG") {
            if (babelAst.message.indexOf("[WARNING]") == 0) {
                fableLib.stdoutLog(babelAst.message);
            }
            else if (opts.verbose) {
                fableLib.stdoutLog(babelAst.message);
            }
        }
        else if (babelAst.type == "ERROR") {
            throw babelAst;
        }
        else if (opts.inMemory || opts.rollup) {
            return fableLib.babelify(babelAst, opts);
        }
        else {
            fableLib.babelifyToFile(babelAst, opts);
        }
    }
    catch (err) {
        fableLib.stderrLog(err);
        if (!opts.watch) {
            fableLib.finish(1, continuation);
        }
    }
}

/** Watches for file changes. Requires chokidar */
function watch(opts, fableProc, parallelProc, continuation) {
    var chokidar = require("chokidar");

    function tooClose(filename, prev) {
        return prev != null &&
            filename == prev[0] &&
            (new Date() - prev[1]) < 3000;
    }
    var next = null, prev = null;
    fableProc.stdin.setEncoding('utf-8');

    var dirs = null;
    if (typeof opts.watch === "string") {
        dirs = [fableLib.pathJoin(opts.workingDir, opts.watch)];
    }
    else if (Array.isArray(opts.watch) && opts.watch.length > 0) {
        dirs = opts.watch.map(dir => fableLib.pathJoin(opts.workingDir, dir));
    }
    else {
        dirs = opts.projFile.map(dir => path.dirname(fableLib.pathJoin(opts.workingDir, dir)));
    }
    fableLib.stdoutLog("Watching " + dirs.join('\n\t'));
    fableLib.stdoutLog("Press Enter to terminate process.");
    opts.watching = true;

    var ready = false;
    var watcher = chokidar
        .watch(dirs, { ignored: /node_modules/, persistent: true })
        .on("ready", function() { ready = true; })
        .on("all", function(ev, filePath) {
            if (ready) {
                var ext = path.extname(filePath).toLowerCase();
                prev = next;
                next = [filePath, new Date()];
                if (!tooClose(filePath, prev)) {
                    fableLib.stdoutLog(ev + ": " + filePath + " at " + next[1].toLocaleTimeString());
                    fableProc.stdin.write(filePath + "\n");
                }
            }
        });

    process.stdin.on('data', function(data) {
        data = data.toString();
        if (data.length > 0 && data[data.length - 1] == '\n') {
            if (parallelProc) {
                parallelProc.kill();
            }
            fableProc.stdin.write("[SIGTERM]\n");
            watcher.close();
            bundleCache = null; // Clean bundle cache just in case
            fableLib.stdoutLog("Process terminated.");
            fableLib.finish(0, continuation);
        }
    });
}

function normalizeProjectName(opts) {
    var projName = path.basename(opts.projFile[opts.projFile.length - 1]);
    return projName.substr(0, projName.indexOf(".")).replace(/[^A-Z_]/ig, "_");
}

var bundleCache = null;
/** Bundles generated JS files and dependencies, requires rollup and plugins */
function bundle(jsFiles, opts, fableProc, continuation) {
    var rollup = require('rollup'),
        hypothetical = require('rollup-plugin-hypothetical');

    var rollupOpts = Object.assign({}, opts.rollup);
    rollupOpts.cache = bundleCache;
    rollupOpts.plugins.splice(0, 0, hypothetical({
        files: jsFiles, allowRealFiles: true, allowExternalModules: true
    }));

    if (rollupOpts.entry == null) {
        rollupOpts.entry = Object.getOwnPropertyNames(jsFiles)
                        .find(function(f) { return jsFiles[f].isEntry });
    }

    fableLib.stdoutLog("Bundling...");
    rollup.rollup(rollupOpts)
        .then(function(bundle) {
            var parsed = bundle.generate(rollupOpts);
            if (opts.inMemory) {
                parsed.fileName = rollupOpts.dest;
                continuation.resolve(parsed);
            }
            else {
                if (opts.watch) {
                    bundleCache = bundle;
                }
                // Write to disk, bundle.write doesn't seem to work
                // bundle.write({ dest: rollupOpts.dest, format: rollupOpts.format, sourceMap: rollupOpts.sourceMap });
                fableLib.writeFile(rollupOpts.dest, parsed.code,
                    rollupOpts.sourceMap === true ? parsed.map : null);
                fableLib.stdoutLog("Bundled " + path.basename(rollupOpts.dest) + " at " + (new Date()).toLocaleTimeString());
                postbuild(opts, true, fableProc, continuation);
            }
        })
        .catch(function (err) {
            fableLib.stderrLog("BUNDLE", err);
            fableLib.finish(1, continuation);
        });
}

/** Runs the postbuild script and starts watching if necessary */
function postbuild(opts, buildSuccess, fableProc, continuation) {
    var parallelProc = null;
    // The "postbuild-once" script must be run only once (well done, Captain Obvious)
    // and it musn't wait till the process is finished, as it's normally used
    // to fire up watch mode of bundlers (Webpack, Rollup...)
    if (buildSuccess && opts.scripts && opts.scripts["postbuild-once"]) {
        var postbuildScript = opts.scripts["postbuild-once"];
        delete opts.scripts["postbuild-once"];
        parallelProc = fableLib.runCommandInParallel(opts.workingDir, postbuildScript);
    }

    // If present, run "postbuild" script after every build and wait till it's finished
    // to exit the process or start watch mode
    if (buildSuccess && opts.scripts && opts.scripts.postbuild) {
        var continuation2 = function (exitCode) {
            if (!opts.watch) {
                fableLib.finish(exitCode, continuation);
            }
            else if (!opts.watching) {
                watch(opts, fableProc, parallelProc, continuation);
            }
        };
        fableLib.runCommand(opts.workingDir, opts.scripts.postbuild)
            .then(continuation2, continuation2);
    }
    else if (!opts.watch) {
        fableLib.finish(0, continuation);
    }
    else if (!opts.watching) {
        watch(opts, fableProc, parallelProc, continuation);
    }
}

/** Builds the project, requires child_process */
function build(opts, continuation) {
    var child_process = require('child_process');

    function wrapInQuotes(arg) {
        if (process.platform === "win32") {
            arg = arg.toString().trim();
            return arg.indexOf(" ") > 0 && arg[0] != '"' ? '"' + arg + '"' : arg;
        }
        else {
            return arg;
        }
    };

    var fableBin = path.resolve(__dirname, "bin/Fable.Client.Node.exe");
    if (constants.PKG_NAME === "fable-compiler-netcore") {
        fableBin = fableBin.replace(".exe", ".dll");
    }

    var fableCmd, fableCmdArgs = [wrapInQuotes(fableBin)]
    if (constants.PKG_NAME === "fable-compiler-netcore") {
        fableCmd = "dotnet";
    }
    else {
        fableCmd = process.platform === "win32" ? null : "mono";
    }

    for (var k in opts) {
        if (constants.FABLE_BIN_OPTIONS.has(k)) {
            if (k === "watch")
                fableCmdArgs.push("--" + k, String(!!opts[k])); // Cast to boolean
            else if (Array.isArray(opts[k]))
                opts[k].forEach(v => fableCmdArgs.push("--" + k, wrapInQuotes(v)))
            else if (typeof opts[k] === "object")
                Object.getOwnPropertyNames(opts[k]).forEach(k2 =>
                    fableCmdArgs.push("--" + k, wrapInQuotes(k2 + "=" + opts[k][k2])))
            else
                fableCmdArgs.push("--" + k, wrapInQuotes(opts[k]));
        }
    }

    if (process.platform === "win32") {
        if (fableCmd) { fableCmdArgs.splice(0, 0, fableCmd); }
        fableCmd = "cmd";
        fableCmdArgs = ["/S", "/C", '"' + fableCmdArgs.join(" ") + '"'];
    }

    // Call Fable.exe
    if (opts.verbose) {
        fableLib.stdoutLog("\nWORKING DIR: " + opts.workingDir) + "\n";
        fableLib.stdoutLog("PROJECT FILE" + (opts.projFile.length > 1 ? "S" : "")  + ": " + opts.projFile.join("; "));
        fableLib.stdoutLog("OUTPUT DIR: " + opts.outDir);
        fableLib.stdoutLog("\nFABLE COMMAND: " + fableCmd + " " + fableCmdArgs.join(" ") + "\n");
    }
    var fableProc = child_process.spawn(fableCmd, fableCmdArgs, { cwd: opts.workingDir, windowsVerbatimArguments: true });

    // Check if dotnet runtime is installed
    // !!child_process.spawnSync("which", ["dotnet"]).stdout.toString()
    // child_process.spawnSync("dotnet", ["--info"]).error != null

    fableProc.on('exit', function(code) {
        // There may be pending messages, do nothing here
    });

    fableProc.stderr.on('data', function(data) {
        fableLib.stderrLog("FABLE", data.toString().substring(0, 300) + "...");
        fableLib.finish(1, continuation);
    });

    var buffer = "", jsFiles = {};
    fableProc.stdout.on("data", function(data) {
        var txt = data.toString(), newLine = 0;
        while (newLine >= 0) {
            var newLine = txt.indexOf("\n");
            if (newLine == -1) {
                buffer += txt;
            }
            else {
                var json = buffer + txt.substring(0, newLine);
                txt = txt.substring(newLine + 1);
                buffer = "";
                var buildFinished = /^\s*\[SIG(SUCCESS|FAIL)\]\s*$/.exec(json);
                if (buildFinished) {
                    var buildSuccess = buildFinished[1] === "SUCCESS";
                    if (opts.rollup && buildSuccess) {
                        bundle(jsFiles, opts, fableProc, continuation);
                    }
                    else if (opts.inMemory) {
                        if (buildSuccess)
                            continuation.resolve(jsFiles);
                        else
                            continuation.reject("Build failed");
                    }
                    else {
                        postbuild(opts, buildSuccess, fableProc, continuation);
                    }
                }
                else {
                    var res = processJson(json, opts, continuation);
                    if (Array.isArray(res))
                        res.forEach(file => jsFiles[file.fileName] = file);
                }
            }
        }
    });
}

function resolvePath(optName, value, workingDir) {
    function resolve(x) {
        return fableLib.pathJoin(workingDir, x)
    }
    // Discard null values or empty strings
    if (value) {
        switch (optName) {
            case "outDir":
                return resolve(value);
            // Multiple values
            case "projFile":
            case "plugins":
            case "babelPlugins":
                return value.map(resolve);
            // Only resolve refs if they starts with '.'
            case "refs":
                var o = {};
                for (var k in value) {
                    o[k] = value[k].startsWith('.') ? resolve(value[k]) : value[k];
                }
                return o;
        }
    }
    return value;
}

/** Reads options from command line, requires command-line-args */
function readCommandLineOptions() {
    function resolveKeyValuePairs(kvs) {
        var o = {};
        for (var i=0; i<kvs.length; i++) {
            var kv = kvs[i].split("=");
            o[kv[0]] = kv[1] || true;
        }
        return o;
    }
    var commandLineArgs = require('command-line-args');
    var opts = commandLineArgs(optionDefinitions);
    if (opts.help) {
        fableLib.stdoutLog(require('command-line-usage')(getAppDescription()));
        fableLib.finish(0);
    }
    if (opts.refs) {
        opts.refs = resolveKeyValuePairs(opts.refs);
    }
    if (opts.coreLib) {
        opts.refs = Object.assign(opts.refs || {}, { "Fable.Core": opts.coreLib })
        delete opts.coreLib;
    }
    if (opts.extra) {
        opts.extra = resolveKeyValuePairs(opts.extra);
    }
    return opts;
}

/** Reads options from fableconfig.json, requires json5 */
function readFableConfigOptions(opts) {
    opts.workingDir = path.resolve(opts.workingDir || process.cwd());
    if (typeof opts.projFile === "string") {
        opts.projFile = [opts.projFile];
    }
    var cfgFile = fableLib.pathJoin(opts.workingDir, constants.FABLE_CONFIG_FILE);

    if (Array.isArray(opts.projFile) && opts.projFile.length === 1) {
        var fullProjFile = fableLib.pathJoin(opts.workingDir, opts.projFile[0]);
        var projDir = fs && fs.statSync(fullProjFile).isDirectory()
                        ? fullProjFile
                        : path.dirname(fullProjFile);
        cfgFile = fableLib.pathJoin(projDir, constants.FABLE_CONFIG_FILE);

        // Delete projFile from opts if it isn't a true F# project
        if (!fableLib.isFSharpProject(fullProjFile)) {
            delete opts.projFile;
        }
    }

    if (fs && fs.existsSync(cfgFile)) {
        // Change workingDir to where fableconfig.json is if necessary
        if (opts.workingDir !== path.dirname(cfgFile)) {
            for (var key in opts) {
                opts[key] = resolvePath(key, opts[key], opts.workingDir);
            }
            opts.workingDir = path.dirname(cfgFile);
        }

        var cfg = require('json5').parse(fs.readFileSync(cfgFile).toString());
        for (var key in cfg) {
            if (key in opts === false)
                opts[key] = cfg[key];
        }
        // Check if a target is requested
        if (opts.debug) { opts.target = "debug" }
        if (opts.production) { opts.target = "production" }
        if (opts.target) {
            if (!opts.targets || !opts.targets[opts.target]) {
                throw "Target " + opts.target + " is missing";
            }
            cfg = opts.targets[opts.target];
            for (key in cfg) {
                if ((typeof cfg[key] === "object") && !Array.isArray(cfg[key]) &&
                    (typeof opts[key] === "object") && !Array.isArray(opts[key])) {
                    for (var key2 in cfg[key])
                        opts[key][key2] = cfg[key][key2];
                }
                else {
                    opts[key] = cfg[key];
                }
            }
        }
    }
    return opts;
}

/** Reads Babel options: plugins and presets */
function readBabelOptions(opts) {
    var babelPresets = [],
        // Add plugins to emit .d.ts files if necessary
        babelPlugins = opts.declaration
        ? [[require("babel-dts-generator"),
            {
                "packageName": "",
                "typings": fableLib.pathJoin(opts.workingDir, opts.outDir),
                "suppressAmbientDeclaration": true,
                "ignoreEmptyInterfaces": false
            }],
            require("babel-plugin-transform-flow-strip-types"),
            require("babel-plugin-transform-class-properties")]
        : [];

    // Add custom plugins
    babelPlugins = babelPlugins.concat(
        customPlugins.transformMacroExpressions,
        // removeUnneededNulls must come after transformMacroExpressions (see #377)
        customPlugins.removeUnneededNulls
    );

    // if opts.babelrc is true, read Babel plugins and presets from .babelrc
    if (opts.babelrc) {
        opts.babel = { presets: babelPresets, plugins: babelPlugins };
        return opts;
    }

    // ECMAScript target
    if (opts.ecma != "es2015" && opts.ecma != "es6") {
        if (opts.module === "es2015" || opts.module === "es6") {
            opts.module = false;
        }
        else if (opts.module in constants.JS_MODULES === false) {
            throw "Unknown module target: " + opts.module;
        }
        babelPresets.push([require.resolve("babel-preset-es2015"), {
            "loose": opts.loose,
            "modules": opts.rollup ? false : opts.module
        }]);
    }
    else if (!opts.rollup && opts.module in constants.JS_MODULES) {
        babelPlugins.push(require("babel-plugin-transform-es2015-modules-" + opts.module));
    }

    // Extra Babel plugins
    if (opts.babelPlugins) {
        babelPlugins = babelPlugins.concat(
            fableLib.resolvePlugins(opts.babelPlugins, opts.workingDir, "babel-plugin-"));
    }

    opts.babel = { presets: babelPresets, plugins: babelPlugins };
    return opts;
}

function readRollupOptions(opts) {
    if (!opts.rollup) {
        return opts;
    }
    var rollupOpts = opts.rollup;
    var outDir = fableLib.pathJoin(opts.workingDir, opts.outDir);

    rollupOpts = typeof rollupOpts === "boolean" ? {} : rollupOpts;
    rollupOpts = typeof rollupOpts === "string" ? { dest: fableLib.pathJoin(outDir, rollupOpts) } : rollupOpts;
    rollupOpts.plugins = fableLib.resolvePlugins(rollupOpts.plugins, opts.workingDir, "rollup-plugin-");

    var plugins = [],
        nodeResolve = 'rollup-plugin-node-resolve',
        commonjs = 'rollup-plugin-commonjs';

    // Attention: Plugin order is important
    if (!rollupOpts.plugins.some(function(kv) { return kv[0].indexOf(nodeResolve) >= 0 })) {
        plugins.push(
            require(nodeResolve)({ ignoreGlobal: true })
        );
    }
    if (!rollupOpts.plugins.some(function(kv) { return kv[0].indexOf(commonjs) >= 0 })) {
        plugins.push(
            require(commonjs)({ jsnext: true, main: true, browser: true })
        );
    }

    // Custom plugins
    for (var i = 0; i < rollupOpts.plugins.length; i++) {
        var kv = rollupOpts.plugins[i];
        plugins.push(require(kv[0])(kv[1]));
    }
    rollupOpts.plugins = plugins;

    // Other options
    rollupOpts.format = rollupOpts.format == null ? constants.JS_MODULES[opts.module] : rollupOpts.format;
    rollupOpts.sourceMap = rollupOpts.sourceMap == null ? opts.sourceMaps : rollupOpts.sourceMap
    rollupOpts.moduleName = rollupOpts.moduleName || normalizeProjectName(opts);
    rollupOpts.dest = rollupOpts.dest == null
        ? fableLib.pathJoin(outDir, "bundle.js")
        : fableLib.pathJoin(opts.workingDir, rollupOpts.dest);

    opts.rollup = rollupOpts;
    return opts;
}

/** Prepares options: read from command line, fableconfig.json, etc */
function readOptions(opts) {
    opts = opts || readCommandLineOptions();
    opts = readFableConfigOptions(opts);

    opts.projFile = Array.isArray(opts.projFile) ? opts.projFile : [opts.projFile];
    for (var i = 0; i < opts.projFile.length; i++) {
        var fullProjFile = fableLib.pathJoin(opts.workingDir, opts.projFile[i] || '');
        if (!fableLib.isFSharpProject(fullProjFile)) {
            throw "Not an F# project (.fsproj) or script (.fsx): " + fullProjFile;
        }
        if (fs && !fs.existsSync(fullProjFile)) {
            throw "Cannot find file: " + fullProjFile;
        }
    }

    // Default values & option processing
    opts.ecma = opts.ecma || "es5";
    opts.outDir = opts.outDir ? opts.outDir : (opts.projFile.length === 1 ? path.dirname(opts.projFile[0]) : ".");
    if (opts.module == null) {
        opts.module = opts.rollup ? "iife" : "es2015";
    }
    if (opts.coreLib) {
        opts.refs = Object.assign(opts.refs || {}, { "Fable.Core": opts.coreLib })
        delete opts.coreLib;
    }
    if (opts.refs) {
        for (var k in opts.refs) {
            var k2 = k.replace(/\.dll$/, "");
            if (k !== k2) {
                opts.refs[k2] = opts.refs[k];
                delete opts.refs[k];
            }
        }
    }

    // Check version
    var curNpmCfg = fableLib.pathJoin(opts.workingDir, "package.json");
    if (!(opts.extra && opts.extra.noVersionCheck) && fs && fs.existsSync(curNpmCfg)) {
        curNpmCfg = JSON.parse(fs.readFileSync(curNpmCfg).toString());
        if (curNpmCfg.engines && (curNpmCfg.engines.fable || curNpmCfg.engines["fable-compiler"])) {
            var semver = require("semver");
            var fableRequiredVersion = curNpmCfg.engines.fable || curNpmCfg.engines["fable-compiler"];
            if (!semver.satisfies(constants.PKG_VERSION, fableRequiredVersion)) {
                throw "Fable version: " + constants.PKG_VERSION + "\n" +
                        "Required: " + fableRequiredVersion + "\n" +
                        "Please upgrade fable-compiler package";
            }
        }
    }

    opts = readBabelOptions(opts);
    opts = readRollupOptions(opts);

    return opts;
}

function main(opts, continuation) {
    fableLib.stdoutLog(constants.PKG_NAME + " " + constants.PKG_VERSION + ": Start compilation...");
    try {
        opts = readOptions(opts);
        if (opts.scripts && opts.scripts.prebuild) {
            var continuation2 = function (exitCode) {
                if (exitCode === 0) { build(opts, continuation); }
                else { fableLib.finish(exitCode, continuation); }
            };
            fableLib.runCommand(opts.workingDir, opts.scripts.prebuild)
                .then(continuation2, continuation2);
        }
        else {
            build(opts, continuation);
        }
    }
    catch (err) {
        fableLib.stderrLog("OPTIONS", err);
        fableLib.finish(1, continuation);
    }
}

/**
 * Starts compilation, if opts is not empty assumes it's
 * running from API and returns a Promise.
*/
exports.compile = function(opts) {
    if (opts) {
        return new Promise(function (resolve, reject) {
            main(opts, { resolve: resolve, reject: reject });
        });
    }
    main(opts);
}
