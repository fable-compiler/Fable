#!/usr/bin/env node

/// <reference path="../../typings/node/node.d.ts" />

var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
var template = require("babel-template");
var child_process = require('child_process');
var commandLineArgs = require('command-line-args');

var appDescription = {
    title: "Fable",
    description: "F# to JavaScript compiler",
    footer: "All arguments can be defined in a fableconfig.json file"
};

var cli = commandLineArgs([
  { name: 'projFile', defaultOption: true, description: "The F# project (.fsproj) or script (.fsx) to compile." },
  { name: 'code', description: "Pass a string of code directly to Fable instead." },
  { name: 'outDir', defaultValue: '.', description: "Where to put compiled JS files." },
  { name: 'sourceMaps', alias: 'm', description: "Generate source maps: [true|inline|false] " },
  { name: 'env', description: "'browser' for [bold]{amd} modules and 'node' for [bold]{commonjs} (defaults to [bold]{umd})." },
  { name: 'lib', defaultValue: '.', description: "Where to find the core library, " +
                        "if not set [underline]{fable-core.js} will be copied automatically to outDir." },
  { name: 'symbols', multiple: true, description: "F# symbols for conditional compilation, like 'DEBUG'." },
  { name: 'plugins', multiple: true, description: "Paths to Fable plugins." },
  { name: 'babelPlugins', multiple: true, description: "Additional Babel plugins (without 'babel-plugin-' prefix, " +
                        "like 'angular2-annotations'). Must be installed in the current directory." },
  { name: 'watch', alias: 'w', type: Boolean, description: "Recompile project much faster on file modifications." },
  { name: 'help', alias: 'h', description: "Display this usage guide." }
]);

var fableBin = path.resolve(__dirname, "bin/Fable.exe");
var fableConfig = "fableconfig.json";
var fableCoreLib = "fable-core.js";

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
    require("babel-plugin-transform-es5-property-mutators"),
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
    var targetFile = path.join(opts.outDir, path.relative(opts.projDir, babelAst.fileName));
    targetFile = targetFile.replace(path.extname(babelAst.fileName), ".js")
    
    var babelOpts = {
        sourceMaps: opts.sourceMaps,
        sourceMapTarget: path.basename(targetFile),
        sourceFileName: path.basename(babelAst.fileName),
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

function postbuild(opts) {
    console.log(opts.scripts.postbuild);
    var cmd, args;
    if (process.platform === "win32") {
        cmd = "cmd";
        args = opts.scripts.postbuild.split(" ").filter(function(x){return x});
        args.splice(0,0,"/C")
    }
    else {
        var i = opts.scripts.postbuild.indexOf(' ');
        cmd = opts.scripts.postbuild.substring(0, i);
        args = opts.scripts.postbuild.substring(i + 1).split(" ").filter(function(x){return x});
    }
    var postProc = child_process.spawn(cmd, args, { cwd: opts.projDir });
    postProc.on('exit', function(code) {
        process.exit(0);
    });
    postProc.stderr.on('data', function(data) {
        console.log(data.toString());
        process.exit(1);
    });
    postProc.stdout.on("data", function(data) {
        console.log(data.toString());
    });
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

try {
    var opts = cli.parse(),
        fableCmd = process.platform === "win32" ? "cmd" : "mono",
        fableCmdArgs = process.platform === "win32" ? ["/C", fableBin] : [fableBin];

    if (opts.help) {
        console.log(cli.getUsage(appDescription));
        process.exit(0);
    }

    opts.projDir = path.resolve(".");
    if (opts.projFile) {
        opts.projDir = path.dirname(
            path.isAbsolute(opts.projFile)
            ? opts.projFile : path.join(process.cwd(), opts.projFile)
        );
        opts.projFile = "./" + path.basename(opts.projFile);
    }
        
    // Parse fableconfig.json
    try {
        var cfgFile = path.join(opts.projDir, fableConfig);
        if (fs.existsSync(cfgFile)) {
            var cfg = JSON.parse(fs.readFileSync(cfgFile).toString());
            for (var key in cfg) {
                opts[key] = cfg[key];
            }
        }
    }
    catch (err) {
        console.log("ERROR: Cannot parse fableconfig file");
        process.exit(1);
    }
    
    if (!opts.projFile && !opts.code) {
        console.log("ERROR: Please provide a F# project (.fsproj) or script (.fsx) file");
        process.exit(1);
    }
    
    opts.outDir = path.isAbsolute(opts.outDir)
                ? opts.outDir
                : path.join(opts.projDir, opts.outDir);
        
    // Copy fable-core.js if lib is "."
    ensureDirExists(opts.outDir);
    if (opts.lib === ".") {
        fs.createReadStream(path.join(__dirname, fableCoreLib))
            .pipe(fs.createWriteStream(path.join(opts.outDir, fableCoreLib)));
    }
        
    // Module target and extra plugins
    if (opts.babelPlugins) {
        opts.babelPlugins.forEach(function (x) {
            babelPlugins.push(require("babel-plugin-" + x));
        });
    }
    if (opts.env === "browser") {
        babelPlugins.push(require("babel-plugin-transform-es2015-modules-amd"));
    }
    else if (opts.env === "node") {
        babelPlugins.push(require("babel-plugin-transform-es2015-modules-commonjs"));
    }
    else {
        babelPlugins.push(require("babel-plugin-transform-es2015-modules-umd"));
    }
    
    // Call Fable.exe
    for (var k in opts) {
        if (Array.isArray(opts[k]))
            opts[k].forEach(function (v) { fableCmdArgs.push("--" + k, v) })
        else if (typeof opts[k] !== "object")
            fableCmdArgs.push("--" + k, opts[k]);
    }
    // console.log(opts.projDir + "> " + fableCmd + " " + fableCmdArgs.join(" "));
        
    var fableProc = child_process.spawn(fableCmd, fableCmdArgs, { cwd: opts.projDir });

    if (opts.watch) {
        fableProc.stdin.setEncoding('utf-8');
        fs.watch(opts.projDir, { persistent: true, recursive: true }, function(ev, filename) {
            var ext = path.extname(filename).toLowerCase();
            if (ev == "change" && (ext == ".fs" || ext == ".fsx")) {
                fableProc.stdin.write(path.join(opts.projDir, filename) + "\n");
            }
        });
    }

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

                // An empty string is the signal to finish the program
                if (/^\s*$/.test(json)) {
                    if (opts.scripts && opts.scripts.postbuild) {
                        fableProc.kill();
                        postbuild(opts);
                    }
                    else {
                        process.exit(0);
                    }
                }
                else {
                    processJson(json, opts);
                }
            }
        }
    });    
}
catch (err) {
    console.log("ARG ERROR: " + err);
    process.exit(1);
}
