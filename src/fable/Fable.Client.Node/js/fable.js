var fableLib = require("./lib");
var constants = require("./constants");
var customPlugins = require("./babelPlugins");

// In the browser this should be set to a falsy value
var fs = require("fs");
var path = require("path") || require("./path");

// Don't use default values as they would block options from fableconfig.json
var optionDefinitions = [
  { name: 'projFile', defaultOption: true, description: "The F# project (.fsproj) or script (.fsx) to compile." },
  { name: 'outDir', alias: 'o', description: "Where to put compiled JS files. Defaults to project directory." },
  { name: 'module', alias: 'm', description: "Specify module code generation: `umd` (default), `commonjs`, `amd` or `es2015`." },
  { name: 'sourceMaps', alias: 's', description: "Generate source maps: `false` (default), `true` or `inline`." },
  { name: 'watch', alias: 'w', type: Boolean, description: "Recompile project much faster on file modifications." },
  { name: 'ecma', description: "Specify ECMAScript target version: `es5` (default) or `es2015`." },
  { name: 'bundle', description: "[EXPERIMENTAL] Bundle files and dependencies and put it in `outDir`, if used as a flag defaults to `bundle.js`." },
  { name: 'symbols', multiple: true, description: "F# symbols for conditional compilation, like `DEBUG`." },
  { name: 'plugins', multiple: true, description: "Paths to Fable plugins." },
  { name: 'babelPlugins', multiple: true, description: "Additional Babel plugins (without `babel-plugin-` prefix). Must be installed in the project directory." },
  { name: 'loose', type: Boolean, description: "Enable “loose” transformations for babel-preset-es2015 plugins (true by default)." },
  { name: 'babelrc', type: Boolean, description: "Use a `.babelrc` file for Babel configuration (invalidates other Babel related options)." },
  { name: 'refs', multiple: true, description: "Specify dll or project references in `Reference=js/import/path` format (e.g. `MyLib=../lib`)." },
  { name: 'dll', type: Boolean, description: "[EXPERIMENTAL] Generate a `dll` assembly." },
  { name: 'msbuild', mutiple: true, description: "Pass MSBuild arguments like `Configuration=Release`." },
  { name: 'noTypedArrays', type: Boolean, description: "Don't compile numeric arrays as JS typed arrays." },  
  { name: 'clamp', type: Boolean, description: "Compile unsigned byte arrays as Uint8ClampedArray." },
  { name: 'copyExt', type: Boolean, description: "Copy external files into `fable_external` folder (true by default)." },
  { name: 'coreLib', description: "In some cases, you may need to pass a different route to the core library, like `--coreLib fable-core/es2015`." },
  { name: 'verbose', type: Boolean, description: "Print more information about the compilation process." },
  { name: 'target', alias: 't', description: "Use options from a specific target in `fableconfig.json`." },
  { name: 'debug', alias: 'd', description: "Shortcut for `--target debug`." },
  { name: 'production', alias: 'p', description: "Shortcut for `--target production`." },
  { name: 'declaration', type: Boolean, description: "[EXPERIMENTAL] Generates corresponding ‘.d.ts’ file." },
  { name: 'extra', multiple: true, description: "Custom options for plugins in `Key=Value` format." },
  { name: 'help', alias: 'h', description: "Display usage guide." }
];

function getAppDescription() {
    return [{ header: 'Fable ' + constants.PKG_VERSION, content: 'F# to JavaScript compiler' },
            { header: 'Options', optionList: optionDefinitions },
            { content: 'All arguments can be defined in a fableconfig.json file' }];
}

/** Processes a JSON received from .NET process. If it's a Babel AST it will be compiled. */
function processJson(json, babelOpts, opts, resolve, reject) {;
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
                fableLib.stderrLog(babelAst.message);
            }
            else if (opts.verbose) {
                fableLib.stdoutLog(babelAst.message);
            }
        }
        else if (babelAst.type == "ERROR") {
            throw babelAst;
        }
        else if (opts.inMemory || opts.bundle) {
            var fsCode = fs && opts.sourceMaps && babelAst.originalFileName
                ? fs.readFileSync(babelAst.originalFileName)
                : null;
            return fableLib.babelify(babelAst, fsCode, babelOpts, opts);
        }
        else {
            fableLib.babelifyToFile(babelAst, babelOpts, opts);
            fableLib.stdoutLog("Compiled " + path.basename(babelAst.fileName) + " at " + (new Date()).toLocaleTimeString());
        }
    }
    catch (e) {
        var err = e.message ? e : { message: e };
        fableLib.stderrLog("ERROR: " + err.message);
        if (opts.verbose && err.stack) {
            fableLib.stderrLog(err.stack);
        }
        if (!opts.watch) {
            fableLib.finish(1, opts, resolve, reject);
        }
    }
}

/** Watches for file changes. Requires chokidar */
function watch(opts, fableProc, parallelProc, resolve) {
    var chokidar = require("chokidar");

    function tooClose(filename, prev) {
        return prev != null &&
            filename == prev[0] &&
            (new Date() - prev[1]) < 1000;
    }
    var next = null, prev = null;
    fableProc.stdin.setEncoding('utf-8');

    // Watch only the project directory for performance
    var projDir = path.dirname(fableLib.pathJoin(opts.workingDir, opts.projFile));
    fableLib.stdoutLog("Watching " + projDir);
    fableLib.stdoutLog("Press Enter to terminate process.");
    opts.watching = true;

    var fsExtensions = [".fs", ".fsx", ".fsproj"];
    var ready = false;
    var watcher = chokidar
        .watch(projDir, { ignored: /node_modules/, persistent: true })
        .on("ready", function() { ready = true; })
        .on("all", function(ev, filePath) {
            if (ready) {
                var ext = path.extname(filePath).toLowerCase();
                if (fsExtensions.indexOf(ext) >= 0) {
                    prev = next;
                    next = [filePath, new Date()];
                    if (!tooClose(filePath, prev)) {
                        fableLib.stdoutLog(ev + ": " + filePath + " at " + next[1].toLocaleTimeString());
                        fableProc.stdin.write(filePath + "\n");
                    }
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
            fableLib.finish(0, opts, resolve);
        }
    });
}

/** Runs a command, requires child_process */
function runCommand(command, opts, continuation) {
    var child_process = require('child_process');
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
    var cmd, args;
    fableLib.stdoutLog(command);
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
    var proc = child_process.spawn(cmd, args, { cwd: opts.workingDir });
    proc.on('exit', function(code) {
        if (typeof continuation === "function")
            continuation(code);
    });
    proc.stderr.on('data', function(data) {
        process.stderr.write(data.toString());
    });
    proc.stdout.on("data", function(data) {
        process.stdout.write(data.toString());
    });
    return proc;
}

function normalizeProjectName(opts) {
    var projName = path.basename(opts.projFile);
    return projName.substr(0, projName.indexOf(".")).replace(/[^A-Z_]/ig, "_");
}

var bundleCache = null;
/** Bundles generated JS files and dependencies, requires rollup and plugins */
function bundle(jsFiles, opts, fableProc, resolve, reject) {
    var rollup = require('rollup'),
        commonjs = require('rollup-plugin-commonjs'),
        nodeResolve = require('rollup-plugin-node-resolve'),
        hypothetical = require('rollup-plugin-hypothetical');

    var defaultRollupPlugins = [
        hypothetical({ files: jsFiles, allowRealFiles: true, allowExternalModules: true }),
        nodeResolve({ jsnext: true, main: true, browser: true }),
        commonjs({ ignoreGlobal: true })
    ];

    var rollupOpts = {};
    if (typeof opts.bundle === "string" && opts.bundle.endsWith("config.js")) {
        var cfgPath = fableLib.pathJoin(opts.workingDir, opts.bundle);
        rollupOpts = require(cfgPath);
        if (rollupOpts.dest) {
            rollupOpts.dest = fableLib.pathJoin(path.dirname(cfgPath), rollupOpts.dest);
        }
    }

    rollupOpts.dest = rollupOpts.dest || fableLib.pathJoin(
        fableLib.pathJoin(opts.workingDir, opts.outDir),
        typeof opts.bundle === "string" ? opts.bundle : "bundle.js"
    ).replace(/\\/g, '/');
    
    rollupOpts.cache = bundleCache;
    rollupOpts.format = rollupOpts.format == null ? constants.JS_MODULES[opts.module] : rollupOpts.format;
    rollupOpts.sourceMap = rollupOpts.sourceMap == null ? opts.sourceMaps : rollupOpts.sourceMap
    rollupOpts.entry = Object.getOwnPropertyNames(jsFiles).find(file => jsFiles[file].isEntry);
    rollupOpts.moduleName = rollupOpts.moduleName || normalizeProjectName(opts);
    rollupOpts.plugins = Array.isArray(rollupOpts.plugins)
        ? defaultRollupPlugins.concat(rollupOpts.plugins)
        : defaultRollupPlugins;

    rollup.rollup(rollupOpts)
        .then(function(bundle) {
            var parsed = bundle.generate(rollupOpts);
            if (opts.inMemory && typeof resolve === "function") {
                parsed.fileName = rollupOpts.dest;
                resolve(parsed);
            }
            else {
                if (opts.watch) {
                    bundleCache = bundle;
                }
                // Write to disk, bundle.write doesn't seem to work
                // bundle.write({ dest: rollupOpts.dest, format: rollupOpts.format, sourceMap: rollupOpts.sourceMap });
                fableLib.writeFile(rollupOpts.dest, parsed.code,
                    rollupOpts.sourceMap === true ? parsed.map : null);
                fableLib.stdoutLog("Compiled " + path.basename(rollupOpts.dest) + " at " + (new Date()).toLocaleTimeString());
                postbuild(opts, true, fableProc, resolve, reject);
            }
        })
        .catch(function (err) {
            fableLib.stderrLog("BUNDLE ERROR: " + err);
            fableLib.finish(1, opts, resolve, reject);
        });
}

/** Runs the postbuild script and starts watching if necessary */
function postbuild(opts, buildSuccess, fableProc, resolve, reject) {
    var parallelProc = null;
    // The "postbuild-once" script must be run only once (well done, Captain Obvious)
    // and it musn't wait till the process is finished, as it's normally used
    // to fire up watch mode of bundlers (Webpack, Rollup...)
    if (buildSuccess && opts.scripts && opts.scripts["postbuild-once"]) {
        var postbuildScript = opts.scripts["postbuild-once"];
        delete opts.scripts["postbuild-once"];
        parallelProc = runCommand(postbuildScript, opts);
    }

    // If present, run "postbuild" script after every build and wait till it's finished
    // to exit the process or start watch mode
    if (buildSuccess && opts.scripts && opts.scripts.postbuild) {
        runCommand(opts.scripts.postbuild, opts, function (exitCode) {
            if (!opts.watch) {
                fableLib.finish(exitCode, opts, resolve, reject);
            }
            else if (!opts.watching) {
                watch(opts, fableProc, parallelProc, resolve);
            }
        });
    }
    else if (!opts.watch) {
        fableLib.finish(0, opts, resolve, reject);
    }
    else if (!opts.watching) {
        watch(opts, fableProc, parallelProc, resolve);
    }
}

/** Builds the project, requires child_process */
function build(opts, resolve, reject) {
    var child_process = require('child_process');    
    var babelOpts = readBabelOptions(opts, resolve, reject);

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
            if (Array.isArray(opts[k]))
                opts[k].forEach(function (v) { fableCmdArgs.push("--" + k, wrapInQuotes(v)) })
            else if (typeof opts[k] !== "object")
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
        fableLib.stdoutLog("\nPROJECT FILE: " + fableLib.pathJoin(opts.workingDir, opts.projFile));
        fableLib.stdoutLog("OUTPUT DIR: " + fableLib.pathJoin(opts.workingDir, opts.outDir));
        fableLib.stdoutLog("WORKING DIR: " + opts.workingDir) + "\n";
        fableLib.stdoutLog("FABLE COMMAND: " + fableCmd + " " + fableCmdArgs.join(" ") + "\n");
    }
    var fableProc = child_process.spawn(fableCmd, fableCmdArgs, { cwd: opts.workingDir, windowsVerbatimArguments: true });

    fableProc.on('exit', function(code) {
        // There may be pending messages, do nothing here
    });

    fableProc.stderr.on('data', function(data) {
        fableLib.stderrLog("FABLE ERROR: " + data.toString().substring(0, 300) + "...");
        fableLib.finish(1, opts, resolve, reject);
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
                    if (opts.bundle && buildSuccess) {
                        bundle(jsFiles, opts, fableProc, resolve, reject);
                    }
                    else if (opts.inMemory && typeof resolve === "function") {
                        if (buildSuccess)
                            resolve(jsFiles);
                        else
                            reject("Build failed");
                    }
                    else {
                        postbuild(opts, buildSuccess, fableProc, resolve, reject);
                    }
                }
                else {
                    var jsFile = processJson(json, babelOpts, opts, resolve, reject);
                    if (jsFile != null)
                        jsFiles[jsFile.fileName] = jsFile;
                }
            }
        }
    });
}

function getExtraOpt(key, opts) {
    if (typeof opts.extra === "string") {
        return opts.extra === key;
    }
    else if (typeof opts.extra === "object") {
        if (Array.isArray(opts.extra)) {
            return opts.extra.indexOf(key) >= 0;
        }
        else {
            return opts.extra[key];
        }
    }
    return null;
}

function resolvePath(optName, value, workingDir) {
    function resolve(x) {
        return fableLib.pathJoin(workingDir, x)
    }
    function resolveArray(arr, f) {
        (Array.isArray(arr) ? arr : [arr]).map(f);
    }
    function resolveKeyValuePairs(kvs) {
        resolveArray(kvs, function (kv) {
            kv = kv.split("=");
            return kv[0] + "=" + resolve(kv[1]);
        })
    }
    // Discard null values or empty strings
    if (value) {
        switch (optName) {
            case "projFile":
            case "outDir":
                return resolve(value);
            // Only resolve coreLib if starts with '.'
            case "coreLib":
                return value.startsWith('.') ? resolve(value) : value;
            case "plugins":
            case "babelPlugins":
                return resolveArray(value, resolve);
            case "refs":
                return resolveKeyValuePairs(value);
        }
    }
    return value;
}

/** Reads options from command line, requires command-line-args */
function readOptionsFromCommandLine() {
    try {
        var commandLineArgs = require('command-line-args');
        var opts = commandLineArgs(optionDefinitions);
        if (opts.help) {
            fableLib.stdoutLog(require('command-line-usage')(getAppDescription()));
            fableLib.finish(0, opts);
        }
        return opts;
    }
    catch (err) {
        throw "Cannot read command line arguments: " + err + "\n" +
                "Use 'fable --help' to see available options";
    }    
}

/** Reads options from fableconfig.json, requires json5 */
function readOptionsFromFableConfig(opts) {
    try {
        opts.workingDir = path.resolve(opts.workingDir || process.cwd());

        var cfgFile = fableLib.pathJoin(opts.workingDir, constants.FABLE_CONFIG_FILE);

        if (opts.projFile) {
            var projFile = fableLib.pathJoin(opts.workingDir, opts.projFile);
            var projDir = fs && fs.statSync(projFile).isDirectory()
                            ? projFile
                            : path.dirname(projFile);
            cfgFile = fableLib.pathJoin(projDir, constants.FABLE_CONFIG_FILE);

            // Delete projFile from opts if it isn't a true F# project
            if (!fableLib.isFSharpProject(opts.projFile)) {
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
                    if (typeof opts[key] == "object") {
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
    catch (err) {
        throw "Cannot parse fableconfig.json: " + err;
    }
}

/** Reads Babel options: plugins and presets */
function readBabelOptions(opts) {
    try {
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
            return { babelPresets: babelPresets, babelPlugins: babelPlugins };
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
                "modules": opts.bundle ? false : opts.module
            }]);
        }
        else if (!opts.bundle && opts.module in constants.JS_MODULES) {
            babelPlugins.push(require("babel-plugin-transform-es2015-modules-" + opts.module));
        }
        
        // Extra Babel plugins
        function resolveBabelPlugin(id) {
            var nodeModulesDir = fableLib.pathJoin(opts.workingDir, "node_modules");
            if (fs && fs.existsSync(fableLib.pathJoin(nodeModulesDir, id))) {
                return fableLib.pathJoin(nodeModulesDir, id);
            }
            else {
                return fableLib.pathJoin(nodeModulesDir, "babel-plugin-" + id);
            }
        }

        if (opts.babelPlugins) {
            (Array.isArray(opts.babelPlugins) ? opts.babelPlugins : [opts.babelPlugins]).forEach(function (plugin) {
                if (typeof plugin === "string") {
                    babelPlugins.push(resolveBabelPlugin(plugin));
                }
                else if (Array.isArray(plugin)) { // plugin id + config obj
                    babelPlugins.push([
                        resolveBabelPlugin(plugin[0]),
                        plugin[1]
                    ]);
                }
                else {
                    throw "Babel plugin must be a string or a [string, object] tuple";
                }
            });
        }

        return { babelPresets: babelPresets, babelPlugins: babelPlugins };
    }
    catch (err) {
        throw "Cannot read Babel options: " + err;
    }
}

/** Prepares options: read from command line, fableconfig.json, etc */
function prepareOptions(opts) {
    opts = opts || readOptionsFromCommandLine();
    opts = readOptionsFromFableConfig(opts);

    if (!fableLib.isFSharpProject(opts.projFile)) {
        throw "Please provide an F# project (.fsproj) or script (.fsx) file";
    }

    var fullProjFile = fableLib.pathJoin(opts.workingDir, opts.projFile);
    if (fs && !fs.existsSync(fullProjFile)) {
        throw "Cannot find project file: " + fullProjFile;
    }

    // Default values
    opts.ecma = opts.ecma || "es5";
    opts.loose = opts.loose != null ? opts.loose : true;
    opts.copyExt = opts.copyExt != null ? opts.copyExt : true;
    opts.coreLib = opts.coreLib || (opts.bundle ? "fable-core/es2015" : "fable-core");
    opts.outDir = opts.outDir ? opts.outDir : path.dirname(opts.projFile);
    if (opts.module == null) {
        opts.module = opts.bundle
            ? "iife"
            : (opts.ecma != "es2015" && opts.ecma != "es6" ? "commonjs" : "es2015");
    }

    // If refs is set in fableconfig.json, convert them to an array
    if (typeof opts.refs == "object" && !Array.isArray(opts.refs)) {
        var refs = [];
        for (var k in opts.refs)
            refs.push(k + "=" + opts.refs[k]);
        opts.refs = refs;
    }

    // Check version
    var curNpmCfg = fableLib.pathJoin(opts.workingDir, "package.json");
    if (!getExtraOpt("noVersionCheck", opts) && fs && fs.existsSync(curNpmCfg)) {
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

    return opts;
}

function main(opts, resolve, reject) {
    try {
        fableLib.stdoutLog(constants.PKG_NAME + " " + constants.PKG_VERSION + ": Start compilation...");

        opts = prepareOptions(opts, resolve, reject);
        if (opts.scripts && opts.scripts.prebuild) {
            runCommand(opts.scripts.prebuild, opts, function (exitCode) {
                if (exitCode == 0) {
                    build(opts, resolve, reject);
                }
                else {
                    fableLib.finish(exitCode, opts, resolve, reject);
                }
            })
        }
        else {
            build(opts, resolve, reject);
        }
    }
    catch (err) {
        fableLib.stderrLog("ERROR: " + err);
        fableLib.finish(1, opts, resolve, reject);
    }
}

/**
 * Starts compilation, if opts is not empty assumes it's
 * running from API and returns a Promise.
*/
exports.run = function(opts) {
    if (opts) {
        return new Promise(function (resolve, reject) {
            main(opts, resolve, reject);
        });
    }
    main(opts);
}
