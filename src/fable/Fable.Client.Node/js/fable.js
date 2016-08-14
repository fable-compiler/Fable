var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
var pkgInfo = require("./package.json");
var template = require("babel-template");
var child_process = require('child_process');
var commandLineArgs = require('command-line-args');
var getUsage = require('command-line-usage');

function getAppDescription() {
    return [
        {
            header: 'Fable ' + pkgInfo.version,
            content: 'F# to JavaScript compiler'
        },
        {
            header: 'Options',
            optionList: optionDefinitions
        },
        {
            content: 'All arguments can be defined in a fableconfig.json file'
        }
    ];
}

// Don't use default values as they would block options from fableconfig.json
var optionDefinitions = [
  { name: 'projFile', defaultOption: true, description: "The F# project (.fsproj) or script (.fsx) to compile." },
  { name: 'outDir', alias: 'o', description: "Where to put compiled JS files. Defaults to project directory." },
  { name: 'module', alias: 'm', description: "Specify module code generation: `umd` (default), `commonjs`, `amd` or `es2015`." },
  { name: 'sourceMaps', alias: 's', description: "Generate source maps: `false` (default), `true` or `inline`." },
  { name: 'watch', alias: 'w', type: Boolean, description: "Recompile project much faster on file modifications." },
  { name: 'ecma', description: "Specify ECMAScript target version: `es5` (default) or `es2015`." },
  { name: 'symbols', multiple: true, description: "F# symbols for conditional compilation, like `DEBUG`." },
  { name: 'plugins', multiple: true, description: "Paths to Fable plugins." },
  { name: 'babelPlugins', multiple: true, description: "Additional Babel plugins (without `babel-plugin-` prefix). Must be installed in the project directory." },
  { name: 'loose', type: Boolean, description: "Enable “loose” transformations for babel-preset-es2015 plugins." },  
  { name: 'refs', multiple: true, description: "Specify dll or project references in `Reference=js/import/path` format (e.g. `MyLib=../lib`)." },
  { name: 'msbuild', mutiple: true, description: "Pass MSBuild arguments like `Configuration=Release`." },
  { name: 'clamp', type: Boolean, description: "Compile unsigned byte arrays as Uint8ClampedArray." },
  { name: 'copyExt', type: Boolean, description: "Copy external files into `fable_external` folder (true by default)." },
  { name: 'coreLib', description: "In some cases, you may need to pass a different route to the core library, like `--coreLib fable-core/es2015`." },
  { name: 'verbose', description: "Print more information about the compilation process." },
  { name: 'target', alias: 't', description: "Use options from a specific target in `fableconfig.json`." },
  { name: 'debug', alias: 'd', description: "Shortcut for `--target debug`." },
  { name: 'production', alias: 'p', description: "Shortcut for `--target production`." },
  { name: 'declaration', type: Boolean, description: "[Experimental] Generates corresponding ‘.d.ts’ file." },
  { name: 'extra', multiple: true, description: "Custom options for plugins in `Key=Value` format." },
  { name: 'help', alias: 'h', description: "Display usage guide." }
];

var cfgDir = process.cwd();
var fableConfig = "fableconfig.json";
var fableBin = path.resolve(__dirname, "bin/Fable.Client.Node.exe");
var fableBinOptions = new Set([
    "projFile", "coreLib", "symbols", "plugins", "msbuild",
    "refs", "watch", "clamp", "copyExt", "extra", "declaration"
]);

// Custom plugin to remove `null;` statements (e.g. at the end of constructors)
var removeNullStatements = {
  visitor: {
    ExpressionStatement: function(path) {
      if (path.node.expression.type == "NullLiteral")
        path.remove();
    }
  }
};

// Custom plugin to simulate macro expressions
var transformMacroExpressions = {
  visitor: {
    StringLiteral: function(path) {
      if (!path.node.macro)
          return;

      try {
        var buildArgs = {}, args = path.node.args;
        for (var i = 0; i < args.length; i++) {
            buildArgs["$" + i] = args[i];
        }

        var tmp = path.node.value
            // Replace spread aguments like in `$0($1...)`
            .replace(/\$(\d+)\.\.\./, function (m, i) {
                var rep = [], j = parseInt(i);
                for (; j < args.length; j++) {
                    rep.push("$" + j);
                }
                return rep.join(",");
            })
            // Replace conditional arguments like in `/$0/g{{$1?i:}}{{$2?m:}}`
            .replace(/\{\{\$(\d+)\?(.*?)\:(.*?)\}\}/g, function (_, g1, g2, g3) {
                var i = parseInt(g1);
                return i < args.length && args[i].value ? g2 : g3;
            })
            // Replace optional arguments like in `$0[$1]{{=$2}}`
            .replace(/\{\{([^\}]*\$(\d+).*?)\}\}/g, function (_, g1, g2) {
                var i = parseInt(g2);
                return i < args.length ? g1 : "";
            });

        var buildMacro = template(tmp);
        path.replaceWithMultiple(buildMacro(buildArgs));
      }
      catch (err) {
          console.log("BABEL ERROR: Failed to parse macro: " + path.node.value);
          console.log(err.message);
          if (opts.verbose && err.stack) {
            console.log(err.stack);
          }
          process.exit(1);
      }
    }
  }
};

var babelPresets = [];
var babelPlugins = [
    transformMacroExpressions,
    removeNullStatements
];

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

function babelifyToFile(babelAst, opts) {
    var projDir = path.dirname(path.resolve(path.join(cfgDir, opts.projFile)));
    var targetFile = path.join(path.resolve(opts.outDir), path.relative(projDir, path.resolve(babelAst.fileName)))
                         .replace(/\\/g, '/')
                         .replace(path.extname(babelAst.fileName), ".js");
    var fsCode = null,
        babelOpts = {
            babelrc: false,
            filename: targetFile,
            sourceRoot: path.resolve(opts.outDir),
            plugins: babelPlugins,
            presets: babelPresets
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

function watch(opts, fableProc) {
    function tooClose(filename, prev) {
        return prev != null &&
            filename == prev[0] &&
            (new Date() - prev[1]) < 1000;
    }
    var next = null, prev = null;
    fableProc.stdin.setEncoding('utf-8');

    // Watch only the project directory for performance
    var projDir = path.dirname(path.resolve(path.join(cfgDir, opts.projFile)));
    console.log("Watching " + projDir);
    console.log("Press Enter to terminate process.");
    opts.watching = true;

    process.stdin.on('data', function(data) {
        data = data.toString();
        if (data.length > 0 && data[data.length - 1] == '\n') {
            console.log("Process terminated.");
            fableProc.stdin.write("[SIGTERM]\n");
            process.exit(0);
        }
    });

    var fsExtensions = [".fs", ".fsx", ".fsproj"];
    fs.watch(projDir, { persistent: true, recursive: true }, function(ev, filename) {
        var ext = path.extname(filename).toLowerCase();
        if (/*ev == "change" &&*/ fsExtensions.indexOf(ext) >= 0) {
            prev = next;
            next = [filename, new Date()];
            if (!tooClose(filename, prev)) {
                console.log(ev + ": " + filename + " at " + next[1].toLocaleTimeString());
                fableProc.stdin.write(path.join(projDir, filename) + "\n");
            }
        }
    });
}

function runCommand(command, continuation) {
    function splitByWhitespace(str) {
        function stripQuotes(str, start, end) {
            return str[start] === '"' && str[end - 1] === '"'
                    ? str.substring(start + 1, end - 1)
                    : str.substring(start, end);
        }
        var reg = /\s+(?=([^"]*"[^"]*")*[^"]*$)/g;
        if (typeof str !== "string" || str.length == 0) {
            throw "'str' argument must be a non-empty string"
        }
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
    console.log(command);
    if (process.platform === "win32") {
        cmd = "cmd";
        args = splitByWhitespace(command);
        args.splice(0,0,"/C")
    }
    else {
        args = splitByWhitespace(command);
        cmd = args[0];
        args = args.slice(1);
    }
    var proc = child_process.spawn(cmd, args, { cwd: cfgDir });
    proc.on('exit', function(code) {
        continuation(code);
    });
    proc.stderr.on('data', function(data) {
        console.log("ERROR: " + data.toString());
    });
    proc.stdout.on("data", function(data) {
        console.log(data.toString());
    });
}

function postbuild(opts, fableProc) {
    if (opts.watch && !opts.watching) {
        watch(opts, fableProc);
    }
    if (opts.scripts && opts.scripts.postbuild) {
        runCommand(opts.scripts.postbuild, function (exitCode) {
            if (!opts.watch)
                process.exit(exitCode);
        });
    }
    else {
        if (!opts.watch)
            process.exit(0);
    }
}

function processJson(json, opts) {
    var err = null;
    try {
        var babelAst;
        try {
            babelAst = JSON.parse(json);
        }
        catch (_err) {
            return; // If console out is not in JSON format, just ignore
        }
        if (babelAst.type == "LOG") {
            if (opts.verbose || babelAst.message.indexOf("[WARNING]") == 0) {
                console.log(babelAst.message);
            }
        }
        else if (babelAst.type == "ERROR") {
            err = babelAst;
        }
        else {
            babelifyToFile(babelAst, opts);
            console.log("Compiled " + path.basename(babelAst.fileName) + " at " + (new Date()).toLocaleTimeString());
        }
    }
    catch (e) {
        err = e.message ? e : { message: e };
    }
    if (err != null) {
        console.log("ERROR: " + err.message);
        if (opts.verbose && err.stack) {
            console.log(err.stack);
        }
        if (!opts.watch) {
            process.exit(1);
        }
    }
}

function build(opts) {
    if (opts.declaration) {
        babelPlugins.splice(0,0,
            [require("babel-dts-generator"),
            {
                "packageName": "",
                "typings": path.resolve(opts.outDir),
                "suppressAmbientDeclaration": true,
                "ignoreEmptyInterfaces": false
            }],
            require("babel-plugin-transform-flow-strip-types"),
            require("babel-plugin-transform-class-properties")
        );
    }

    // ECMAScript target
    if (opts.ecma != "es2015" && opts.ecma != "es6") {
        opts.module = opts.module || "umd"; // Default module
        if (opts.module === "es2015" || opts.module === "es6") {
            opts.module = false;
        }
        else if (["amd", "commonjs", "systemjs", "umd"].indexOf(opts.module) == -1) {
            throw "Unknown module target: " + opts.module;
        }
        babelPresets.push([require.resolve("babel-preset-es2015"), {
            "loose": opts.loose || false,
            "modules": opts.module
        }]);
    }
    
    // Extra Babel plugins
    function resolveBabelPlugin(id) {
        var nodeModulesDir = path.join(path.resolve(cfgDir), "node_modules");
        if (fs.existsSync(path.join(nodeModulesDir, id))) {
            return path.join(nodeModulesDir, id);
        }
        else {
            return path.join(nodeModulesDir, "babel-plugin-" + id);
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

    var wrapInQuotes = function (arg) {
        if (process.platform === "win32") {
            arg = arg.toString().trim();
            return arg.indexOf(" ") > 0 && arg[0] != '"' ? '"' + arg + '"' : arg;
        }
        else {
            return arg;
        }
    };
    var fableCmd = "mono", fableCmdArgs = [wrapInQuotes(fableBin)];

    for (var k in opts) {
        if (fableBinOptions.has(k)) {
            if (Array.isArray(opts[k]))
                opts[k].forEach(function (v) { fableCmdArgs.push("--" + k, wrapInQuotes(v)) })
            else if (typeof opts[k] !== "object")
                fableCmdArgs.push("--" + k, wrapInQuotes(opts[k]));
        }
    }

    if (process.platform === "win32") {
        fableCmd = "cmd";
        fableCmdArgs = ["/S", "/C", '"' + fableCmdArgs.join(" ") + '"'];
    }

    // Call Fable.exe
    if (opts.verbose) {
        console.log("\nPROJECT FILE: " + path.resolve(path.join(cfgDir, opts.projFile)));
        console.log("OUTPUT DIR: " + path.resolve(opts.outDir));
        console.log("WORKING DIR: " + path.resolve(cfgDir) + "\n");
        console.log("FABLE COMMAND: " + fableCmd + " " + fableCmdArgs.join(" ") + "\n");
    }
    console.log("Fable " + pkgInfo.version + ": Start compilation...");
    var fableProc = child_process.spawn(fableCmd, fableCmdArgs, { cwd: cfgDir, windowsVerbatimArguments: true });

    fableProc.on('exit', function(code) {
        // There may be pending messages, do nothing here
    });

    fableProc.stderr.on('data', function(data) {
        console.log("FABLE ERROR: " + data.toString().substring(0, 300) + "...");
        process.exit(1);
    });

    var buffer = "";
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

                // An empty string is the signal of the end of compilation
                if (/^\s*$/.test(json)) {
                    postbuild(opts, fableProc);
                }
                else {
                    processJson(json, opts);
                }
            }
        }
    });
}

// Init
try {
    var opts = commandLineArgs(optionDefinitions);
    if (opts.help) {
        console.log(getUsage(getAppDescription()));
        process.exit(0);
    }

    if (opts.projFile) {
        if (fs.statSync(opts.projFile).isDirectory()) {
            cfgDir = opts.projFile;
            delete opts.projFile;
        }
        else {
            cfgDir = path.dirname(opts.projFile);
            opts.projFile = path.basename(opts.projFile);
        }
    }

    // Parse fableconfig.json if present
    try {
        var cfgFile = path.join(cfgDir, fableConfig);
        if (fs.existsSync(cfgFile)) {
            var cfg = JSON.parse(fs.readFileSync(cfgFile).toString());
            for (var key in cfg)
                if (key in opts == false)
                    opts[key] = cfg[key];

            // Check if a target is requested
            if (opts.debug) { opts.target = "debug" }
            if (opts.production) { opts.target = "production" }
            if (opts.target) {
                if (!opts.targets || !opts.targets[opts.target])
                    throw "Target " + opts.target + " is missing";

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
    }
    catch (err) {
        console.log("ERROR: Cannot parse fableconfig.json: " + err);
        process.exit(1);
    }

    // Default values
    opts.copyExt = "copyExt" in opts ? opts.copyExt : true;
    opts.outDir = opts.outDir ? (path.join(cfgDir, opts.outDir)) : path.dirname(path.join(cfgDir, opts.projFile));
    opts.ecma = opts.ecma || "es5";
    if (typeof opts.refs == "object" && !Array.isArray(opts.refs)) {
        var refs = [];
        for (var k in opts.refs)
            refs.push(k + "=" + opts.refs[k]);
        opts.refs = refs;
    }

    if ([".fsproj", ".fsx"].indexOf(path.extname(opts.projFile)) == -1 ) {
        console.log("ERROR: Please provide a F# project (.fsproj) or script (.fsx) file");
        console.log("Use 'fable --help' to see available options");
        process.exit(1);
    }

    if (!fs.existsSync(path.resolve(path.join(cfgDir, opts.projFile)))) {
        console.log("ERROR: Cannot find project file: " + opts.projFile);
        process.exit(1);
    }

    // Check version
    var curNpmCfg = path.join(cfgDir, "package.json");
    if (fs.existsSync(curNpmCfg)) {
        curNpmCfg = JSON.parse(fs.readFileSync(curNpmCfg).toString());
        if (curNpmCfg.engines && (curNpmCfg.engines.fable || curNpmCfg.engines["fable-compiler"])) {
            var semver = require("semver");
            var fableRequiredVersion = curNpmCfg.engines.fable || curNpmCfg.engines["fable-compiler"];
            if (!semver.satisfies(pkgInfo.version, fableRequiredVersion)) {
                console.log("Fable version: " + pkgInfo.version);
                console.log("Required: " + fableRequiredVersion);
                console.log("Please upgrade fable-compiler package");
                process.exit(1);
            }
        }
    }
    
    if (opts.verbose) {
        console.log("Fable F# to JS compiler version " + pkgInfo.version);
    }

    if (opts.scripts && opts.scripts.prebuild) {
        runCommand(opts.scripts.prebuild, function (exitCode) {
            if (exitCode == 0)
                build(opts);
            else
                process.exit(exitCode);
        })
    }
    else {
        build(opts);
    }
}
catch (err) {
    console.log("ARG ERROR: " + err);
    console.log("Use 'fable --help' to see available options");
    process.exit(1);
}
