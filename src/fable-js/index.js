#!/usr/bin/env node
/* global __dirname */
/* global process */

var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
var template = require("babel-template");
var spawn = require('child_process').spawn;

var fableBin = path.resolve(__dirname, "bin/Fable.exe");
var fableConfig = "fableconfig.json";
var fableCoreLib = "fable-core.js";

// Custom plugin to simulate macro expressions
var transformMacroExpressions = {
  visitor: {
    StringLiteral(path) {
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
    VariableDeclaration(path) {
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

function babelifyToFile(projDir, projectDir, babelAst) {
    var targetFile = path.join(projectDir, path.relative(projDir, babelAst.fileName));
    targetFile = targetFile.replace(path.extname(babelAst.fileName), ".js")
    
    var opts = {
        sourceMaps: true,
        sourceMapTarget: path.basename(targetFile),
        sourceFileName: babelAst.fileName,
        plugins: babelPlugins
    };

    var parsed = babel.transformFromAst(babelAst, null, opts);
    
    ensureDirExists(path.dirname(targetFile));
    fs.writeFileSync(targetFile, parsed.code);
    fs.appendFileSync(targetFile, "\n//# sourceMappingURL=" + path.basename(targetFile)+".map");
    
    fs.writeFileSync(targetFile + ".map", JSON.stringify(parsed.map));
}

try {
    var opts = {
        lib: ".",
        outDir: ".",
        projectDir: ".",
        symbols: [],
        plugins: [],
        watch: false
    }
    
    // TODO: Show help if no arguments passed

    for (var i=2; i < process.argv.length; i++) {
        var key = process.argv[i];
        if (i == 2 && key.indexOf("--") != 0) {
            opts.projFile = key;    
        }
        else {
            key = key.substring(2);
            if (key == "watch") {
                opts[key] = true;
            }
            else if (Array.isArray(opts[key])) {
                opts[key].push(process.argv[++i]);
            }
            else {
                opts[key] = process.argv[++i];
            }
        }
    }
    
    var projectDir;
    var fableCmd = process.platform === "win32" ? "cmd" : "mono";
    var fableCmdArgs = process.platform === "win32" ? ["/C", fableBin] : [fableBin];

    if (typeof opts.projFile === "string") {
        projectDir = path.dirname(path.isAbsolute(opts.projFile)
                    ? opts.projFile
                    : path.join(process.cwd(), opts.projFile));
        opts.projFile = "./" + path.basename(opts.projFile);
        
        try {
            var cfgFile = path.join(projectDir, fableConfig);
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
        opts.outDir = path.isAbsolute(opts.outDir)
                    ? opts.outDir
                    : path.join(projectDir, opts.outDir);
        
        // Copy fable-core.js if set to "." but not present in outDir
        ensureDirExists(opts.outDir);
        if (opts.lib === "." && !fs.existsSync(path.join(opts.outDir, fableCoreLib))) {
            fs.createReadStream(path.join(__dirname, fableCoreLib))
                .pipe(fs.createWriteStream(path.join(opts.outDir, fableCoreLib)));
        }
    }
    else if (typeof opts.code !== "string") {
        throw "No correct --projFile or --code argument provided";
    }
        
    // Module target
    if (opts.env === "browser") {
        babelPlugins.push(require("babel-plugin-transform-es2015-modules-amd"));
    }
    else if (opts.env === "node") {
        babelPlugins.push(require("babel-plugin-transform-es2015-modules-commonjs"));
    }
    else {
        babelPlugins.push(require("babel-plugin-transform-es2015-modules-umd"));
    }
    
    var addArg = function(k, v) {
        if (v != null) {
            fableCmdArgs.push("--" + k, v.toString());
        }
    };
    for (var k in opts) {
        if (Array.isArray(opts[k])) {
            opts[k].forEach(function (v) {
                addArg(k,v);
            });
        }
        else {
            addArg(k, opts[k]);
        }
    }
    console.log(projectDir + "> " + fableCmd + " " + fableCmdArgs.join(" "));
    
    var proc = spawn(fableCmd, fableCmdArgs, { cwd: projectDir });

    if (opts.watch) {
        proc.stdin.setEncoding('utf-8');
        fs.watch(projectDir, { persistent: true, recursive: true }, function(ev, filename) {
            var ext = path.extname(filename).toLowerCase();
            if (ev == "change" && (ext == ".fs" || ext == ".fsx")) {
                proc.stdin.write(path.join(projectDir, filename) + "\n");
            }
        });
    }

    proc.on('exit', function(code) {
        // Don't exit the process here as there may be pending messages
    });    

    proc.stderr.on('data', function(data) {
        console.log("FABLE ERROR: " + data.toString().substring(0, 300) + "...");
        process.exit(1);
    });    

    var buffer = "";
    proc.stdout.on("data", function(data) {
        var txt = data.toString();
        // New lines are parsed as literals \n,
        // so this complicated RegExp isn't necessary
        // var json, closing = /\}\n(?![^$\{][\s\S]*")/.exec(txt);
        var json, closing = txt.indexOf("\n");
        if (closing == -1) {
            buffer += txt;
            return;
        }
        else {
            json = buffer + txt.substring(0, closing + 1);
            buffer = txt.substring(closing + 1);
        }
        
        // An empty string is the signal to finish the program
        if (/^\s*$/.test(json)) {
            console.log("Finished");
            process.exit(0);
        }
        
        var err = null;
        try {
            var babelAst = JSON.parse(json);
            if (babelAst.type == "Error") {
                err = babelAst.message;
            }
            else {
                if (typeof opts.projFile === "string") {
                    babelifyToFile(projectDir, opts.outDir, babelAst);
                    console.log("Compiled " + path.basename(babelAst.fileName) + " at " + (new Date()).toLocaleTimeString());
                }
                else {
                    babelifyToConsole(babelAst);
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
    });    
}
catch (err) {
    console.log("ARG ERROR: " + err);
    process.exit(1);
}
