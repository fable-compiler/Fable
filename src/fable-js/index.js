#!/usr/bin/env node

/// <reference path="../../typings/node/node.d.ts" />

var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
var template = require("babel-template");
var child_process = require('child_process');
var commandLineArgs = require('command-line-args');

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
  { name: 'refs', multiple: true, description: "Specify project references in `Project=js/import/path` format (e.g. `MyLib=../lib`)." },
  { name: 'clamp', type: Boolean, description: "Compile unsigned byte arrays as Uint8ClampedArray." },
  { name: 'target', alias: 't', description: "Use options from a specific target in `fableconfig.json`." },
  { name: 'debug', alias: 'd', description: "Shortcut for `--target debug`." },
  { name: 'production', alias: 'p', description: "Shortcut for `--target production`." },
  { name: 'code', description: "Pass a string of code directly to Fable." },
  { name: 'env', description: "[Deprecated] Use --module instead." },
  { name: 'help', alias: 'h', description: "Display usage guide." }
]);

var fableBin = path.resolve(__dirname, "bin/Fable.exe");
var fableConfig = "fableconfig.json";

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

function babelifyToConsole(babelAst) {
    var opts = { plugins: babelPlugins };
    var parsed = babel.transformFromAst(babelAst, null, opts);
    console.log(parsed.code);
}

function babelifyToFile(babelAst, opts) {
    var projDir = path.dirname(opts.projFile);
    var targetFile = path.join(opts.outDir, path.relative(projDir, babelAst.fileName));
    targetFile = targetFile.replace(path.extname(babelAst.fileName), ".js")

    var babelOpts = {
        sourceMaps: opts.sourceMaps,
        sourceMapTarget: path.basename(targetFile),
        sourceFileName: path.relative(path.dirname(targetFile), babelAst.fileName).replace(/\\/g, '/'),
        plugins: babelPlugins
    };

    // The F# code is only necessary when generating source maps
    var fsCode = opts.sourceMaps
        ? fs.readFileSync(babelAst.fileName) : null;

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
    var prev = null;
    fableProc.stdin.setEncoding('utf-8');

    // Watch only the project directory for performance
    var projDir = path.dirname(opts.projFile);
    console.log("Watching " + (projDir || '.'));

    var fsExtensions = [".fs", ".fsx", ".fsproj"];
    fs.watch(projDir, { persistent: true, recursive: true }, function(ev, filename) {
        var ext = path.extname(filename).toLowerCase();
        if (/*ev == "change" &&*/ fsExtensions.indexOf(ext) >= 0) {
            filename = path.join(projDir, filename);
            if (!tooClose(filename, prev)) {
                fableProc.stdin.write(filename + "\n");
            }
            prev = [filename, new Date()];
        }
    });
}

function runCommand(command, continuation) {
    var cmd, args;
    console.log(command);
    if (process.platform === "win32") {
        cmd = "cmd";
        args = command.split(" ").filter(function(x){return x});
        args.splice(0,0,"/C")
    }
    else {
        var i = command.indexOf(' ');
        cmd = command.substring(0, i);
        args = command.substring(i + 1).split(" ").filter(function(x){return x});
    }
    var proc = child_process.spawn(cmd, args, { cwd: process.cwd() });
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
    if (opts.watch) {
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
        var babelAst = JSON.parse(json);
        if (babelAst.type == "Error") {
            err = babelAst.message;
        }
        else {
            // When a code string is passed, just display the result on screen
            if (opts.code) {
                babelifyToConsole(babelAst);
            }
            else {
                babelifyToFile(babelAst, opts);
                console.log("Compiled " + path.basename(babelAst.fileName) + " at " + (new Date()).toLocaleTimeString());
            }
        }
    }
    catch (e) {
        err = e;
    }
    if (err != null) {
        console.log(err);
        if (!opts.watch) {
            process.exit(1);
        }
    }
}

function addModulePlugin(opts, babelPlugins) {
    // Legacy
    if (!opts.module && opts.env) {
        opts.module = opts.env == "browser" ? "amd" : "commonjs";
    }

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
            babelPlugins.push(require(path.join(process.cwd(), "node_modules", "babel-plugin-" + x)));
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
    // console.log(process.cwd() + "> " + fableCmd + " " + fableCmdArgs.join(" "));
    console.log("Start compilation...");
    var fableProc = child_process.spawn(fableCmd, fableCmdArgs, { cwd: process.cwd() });

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

    // Parse fableconfig.json if present
    try {
        var cfgFile = path.join(process.cwd(), fableConfig);
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
    opts.outDir = opts.outDir || path.dirname(opts.projFile);
    opts.ecma = opts.ecma || "es5";
    if (typeof opts.refs == "object" && !Array.isArray(opts.refs)) {
        var refs = [];
        for (var k in opts.refs)
            refs.push(k + "=" + opts.refs[k]);
        opts.refs = refs;
    }

    if (!opts.code && [".fsproj", ".fsx"].indexOf(path.extname(opts.projFile)) == -1 ) {
        console.log("ERROR: Please provide a F# project (.fsproj) or script (.fsx) file");
        console.log("Use 'fable --help' to see available options");
        process.exit(1);
    }

    // Check version
    var curNpmCfg = path.join(process.cwd(), "package.json");
    if (fs.existsSync(curNpmCfg)) {
        curNpmCfg = require(curNpmCfg);
        if (curNpmCfg.engines && curNpmCfg.engines.fable) {
            var semver = require("semver");
            var fableVersion = require("./package.json").version;
            if (!semver.satisfies(fableVersion, curNpmCfg.engines.fable)) {
                console.log("Fable version: " + fableVersion);
                console.log("Required: " + curNpmCfg.engines.fable);
                console.log("Please upgrade fable-compiler package");
                process.exit(1);
            }
        }
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
