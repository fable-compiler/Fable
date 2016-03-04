/* global __dirname */
/* global process */

var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
var template = require("babel-template");
var spawn = require('child_process').spawn;

var fableBinDir = path.resolve(__dirname, ["..", "build", "main"].join(path.sep));
var fableBinExe = "Fable.exe";
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
    "transform-do-expressions",
    removeDuplicatedVarDeclarators,
    "transform-es5-property-mutators",
    "transform-es2015-arrow-functions",
    "transform-es2015-classes",
    "transform-es2015-computed-properties",
    "transform-es2015-for-of",
    "transform-es2015-object-super",
    "transform-es2015-parameters",
    "transform-es2015-shorthand-properties",
    "transform-es2015-spread"
];

function ensureDirExists(dir, cont) {
    if (fs.existsSync(dir)) {
        if (typeof cont === "function") { cont(); }
    }
    else {
        ensureDirExists(path.dirname(dir), function() {
            fs.mkdirSync(dir);
            if (typeof cont === "function") { cont(); }
        })
    }
}

function babelifyToConsole(babelAst) {
    var opts = { plugins: babelPlugins };
    var parsed = babel.transformFromAst(babelAst, null, opts);
    console.log(parsed.code);
}

function babelifyToFile(projDir, outDir, babelAst) {
    var targetFile = path.join(outDir, path.relative(projDir, babelAst.fileName));
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
    
    var fableCwd = process.cwd();
    var fableCmd = process.platform === "win32" ? "cmd" : "mono";
    var fableCmdArgs = [path.join(fableBinDir, fableBinExe)];
    if (process.platform === "win32") {
        fableCmdArgs.unshift("/C");
    }

    if (typeof opts.projFile === "string") {
        fableCwd = path.dirname(path.isAbsolute(opts.projFile) ? opts.projFile : path.join(fableCwd, opts.projFile));
        opts.projFile = "./" + path.basename(opts.projFile);
        
        try {
            var cfgFile = path.join(fableCwd, fableConfig);
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
    }
    else if (typeof opts.code !== "string") {
        throw "No correct --projFile or --code argument provided";
    }
    
    // Copy fable-core.js if not present
    if (opts.projFile && opts.lib === "." && !fs.existsSync(path.join(fableCwd, fableCoreLib))) {
        fs.createReadStream(path.join(fableBinDir, fableCoreLib))
            .pipe(fs.createWriteStream(path.join(fableCwd, fableCoreLib)));
    }
    
    // Module target
    if (opts.env === "browser") {
        babelPlugins.push("transform-es2015-modules-amd");
    }
    else if (opts.env === "node") {
        babelPlugins.push("transform-es2015-modules-commonjs");
    }
    else {
        babelPlugins.push("transform-es2015-modules-umd");
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
    console.log(fableCwd + "> " + fableCmd + " " + fableCmdArgs.join(" "));
    
    var proc = spawn(fableCmd, fableCmdArgs, { cwd: fableCwd });

    if (opts.watch) {
        proc.stdin.setEncoding('utf-8');
        fs.watch(fableCwd, { persistent: true, recursive: true }, function(ev, filename) {
            var ext = path.extname(filename).toLowerCase();
            if (ev == "change" && (ext == ".fs" || ext == ".fsx")) {
                proc.stdin.write(path.join(fableCwd, filename) + "\n");
            }
        });
    }

    proc.on('exit', function(code) {
        console.log("Finished");
        process.exit(code);
    });    

    proc.stderr.on('data', function(data) {
        console.log("FABLE ERROR: " + data.toString().substring(0, 300) + "...");
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
        
        var err = null;
        try {
            var babelAst = JSON.parse(json);
            if (babelAst.type == "Error") {
                err = babelAst.message;
            }
            else {
                if (opts.projFile) {
                    babelifyToFile(fableCwd, path.join(fableCwd, opts.outDir), babelAst);
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
