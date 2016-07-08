/// <reference path="../../typings/node/node.d.ts" />

var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
var template = require("babel-template");
var child_process = require('child_process');
var commandLineArgs = require('command-line-args');

var cfgDir = process.cwd();

function getAppDescription() {
    return {
        title: "Fable " + require("./package.json").version,
        description: "F# to JavaScript compiler",
        footer: "All arguments can be defined in a fableconfig.json file"
    };
}

// Don't use default values as they would block options from fableconfig.json
var cli = commandLineArgs([
  { name: 'projFile', defaultOption: true, description: "The F# project (.fsproj) or script (.fsx) to compile." },
  { name: 'outDir', alias: 'o', description: "Where to put compiled JS files. Defaults to project directory." },
  { name: 'module', alias: 'm', description: "Specify module code generation: `umd` (default), `commonjs`, `amd` or `es2015`." },
  { name: 'sourceMaps', alias: 's', description: "Generate source maps: `false` (default), `true` or `inline`." },
  { name: 'watch', alias: 'w', type: Boolean, description: "Recompile project much faster on file modifications." },
  { name: 'ecma', description: "Specify ECMAScript target version: `es5` (default) or `es2015`." },
  { name: 'symbols', multiple: true, description: "F# symbols for conditional compilation, like `DEBUG`." },
  { name: 'plugins', multiple: true, description: "Paths to Fable plugins." },
  { name: 'babelPlugins', multiple: true, description: "Additional Babel plugins (without `babel-plugin-` prefix). Must be installed in the project directory." },
  { name: 'refs', multiple: true, description: "Specify dll or project references in `Reference=js/import/path` format (e.g. `MyLib=../lib`)." },
  { name: 'msbuild', mutiple: true, description: "Pass MSBuild arguments like `Configuration=Release`." },
  { name: 'clamp', type: Boolean, description: "Compile unsigned byte arrays as Uint8ClampedArray." },
  { name: 'copyExt', type: Boolean, defaultValue: true, description: "Copy external files into `fable_external` folder (true by default)." },
  { name: 'coreLib', description: "In some cases, you may need to pass a different route to the core library, like `--coreLib fable-core/es2015`." },
  { name: 'verbose', description: "Print more information about the compilation process." },
  { name: 'target', alias: 't', description: "Use options from a specific target in `fableconfig.json`." },
  { name: 'debug', alias: 'd', description: "Shortcut for `--target debug`." },
  { name: 'production', alias: 'p', description: "Shortcut for `--target production`." },
  { name: 'help', alias: 'h', description: "Display usage guide." }
]);

var fableBin = path.resolve(__dirname, "bin/Fable.Client.Node.exe");
var fableConfig = "fableconfig.json";

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
          process.exit(1);
      }
    }
  }
};

// In pattern matching targets, some variables may have the same name
// but we can safely remove the duplicates after do expressions have
// been resolved and the variable declarations hoisted
var removeDuplicatedVarDeclarators = {
  visitor: {
    VariableDeclaration: function(path) {
      var buffer = [];
      var duplicated = [];

      for (var i = 0; i < path.node.declarations.length; i++) {
          var decl = path.node.declarations[i];
          if (typeof decl.id.name === "string") {
              if (buffer.indexOf(decl.id.name) > -1 && decl.init == null) {
                  duplicated.push(i);
              }
              else {
                  buffer.push(decl.id.name);
              }
          }
      }

      try {
        if (duplicated.length > 0) {
            var node = path.node;
            for (var j = duplicated.length - 1; j >= 0; j--) {
                node.declarations.splice(duplicated[j], 1);
            }
            path.replaceWith(node);
        }
      }
      catch (err) {
          console.log("BABEL ERROR: Failed to remove duplicated variables");
          process.exit(1);
      }
    }
  }
};

var babelPlugins = [
    transformMacroExpressions,
    // "transform-es2015-block-scoping", // This creates too many function wrappers
    require("babel-plugin-transform-do-expressions"),
    removeNullStatements,
    removeDuplicatedVarDeclarators,
    // If this plugins is not used, glitches may appear in final code 
    require("babel-plugin-transform-es5-property-mutators"),
];

var babelPlugins_es2015 = [
    require("babel-plugin-transform-es2015-arrow-functions"),
    require("babel-plugin-transform-es2015-classes"),
    require("babel-plugin-transform-es2015-computed-properties"),
    require("babel-plugin-transform-es2015-for-of"),
    require("babel-plugin-transform-es2015-object-super"),
    require("babel-plugin-transform-es2015-parameters"),
    require("babel-plugin-transform-es2015-shorthand-properties"),
    require("babel-plugin-transform-es2015-spread")
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
    var targetFile = path.join(opts.outDir, path.relative(projDir, path.resolve(babelAst.fileName)));
    targetFile = targetFile.replace(path.extname(babelAst.fileName), ".js")

    var babelOpts = {
        sourceMaps: opts.sourceMaps,
        sourceMapTarget: path.basename(targetFile),
        sourceFileName: path.relative(path.dirname(targetFile), babelAst.originalFileName).replace(/\\/g, '/'),
        plugins: babelPlugins
    };

    // The F# code is only necessary when generating source maps
    var fsCode = opts.sourceMaps
        ? fs.readFileSync(babelAst.originalFileName) : null;

    var parsed = babel.transformFromAst(babelAst, fsCode, babelOpts);
    ensureDirExists(path.dirname(targetFile));
    fs.writeFileSync(targetFile, parsed.code);

    // Use strict equality so it evals to false when opts.sourceMaps === "inline"
    if (opts.sourceMaps === true) {
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
    opts.watching = true;

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
        // if (!(reg instanceof RegExp) || reg.flags.indexOf("g") == -1) {
        //     throw "'reg' argument must be a RegExp with 'g' flag"
        // }
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
        err = { message: e };
    }
    if (err != null) {
        console.log("ERROR: " + err.message);
        if (opts.verbose && err.stackTrace) {
            console.log(err.stackTrace);
        }
        if (!opts.watch) {
            process.exit(1);
        }
    }
}

function addModulePlugin(opts, babelPlugins) {
    // Default
    opts.module = opts.module || "umd";

    if (["amd", "commonjs", "umd"].indexOf(opts.module) >= 0) {
        babelPlugins.push(require("babel-plugin-transform-es2015-modules-" + opts.module));
    }
    else if (opts.module === "es2015" || opts.module === "es6") {
        // Do nothing
    }
    else {
        throw "Unknown module target: " + opts.module;
    }
}

function build(opts) {
    // ECMAScript target
    if (opts.ecma != "es2015" && opts.ecma != "es6") {
        babelPlugins = babelPlugins.concat(babelPlugins_es2015);
    }
    
    // Extra Babel plugins
    if (opts.babelPlugins) {
        (Array.isArray(opts.babelPlugins) ? opts.babelPlugins : [opts.babelPlugins]).forEach(function (x) {
            babelPlugins.push(require(path.join(path.resolve(cfgDir), "node_modules", "babel-plugin-" + x)));
        });
    }

    // Module target
    addModulePlugin(opts, babelPlugins);

    var fableCmd = process.platform === "win32" ? "cmd" : "mono";
    var fableCmdArgs = process.platform === "win32" ? ["/C", fableBin] : [fableBin];

    for (var k in opts) {
        if (Array.isArray(opts[k]))
            opts[k].forEach(function (v) { fableCmdArgs.push("--" + k, v) })
        else if (typeof opts[k] !== "object")
            fableCmdArgs.push("--" + k, opts[k]);
    }

    // Call Fable.exe
    if (opts.verbose) {
        console.log("PROJECT FILE: " + path.resolve(path.join(cfgDir, opts.projFile)) + "\n");
        console.log("OUTPUT DIR: " + path.resolve(opts.outDir) + "\n");
        console.log("WORKING DIR: " + path.resolve(cfgDir) + "\n");
        console.log("FABLE COMMAND: " + fableCmd + " " + fableCmdArgs.join(" ") + "\n");
    }
    console.log("Start compilation...");
    var fableProc = child_process.spawn(fableCmd, fableCmdArgs, { cwd: cfgDir });

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
    var opts = cli.parse();
    if (opts.help) {
        console.log(cli.getUsage(getAppDescription()));
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
            var fableVersion = require("./package.json").version;
            if (!semver.satisfies(fableVersion, fableRequiredVersion)) {
                console.log("Fable version: " + fableVersion);
                console.log("Required: " + fableRequiredVersion);
                console.log("Please upgrade fable-compiler package");
                process.exit(1);
            }
        }
    }
    
    if (opts.verbose) {
        console.log("Fable F# to JS compiler version " + require("./package.json").version);
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
