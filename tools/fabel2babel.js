/* global __dirname */
/* global process */

var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
var template = require("babel-template");
var spawn = require('child_process').spawn;

// Custom plugin to simulate macro expressions
var transformMacroExpressions = {
  visitor: {
    StringLiteral(path) {
      if (!path.node.macro)
          return;
  
      try {
        var buildArgs = {};
        var buildMacro = template(path.node.value);
        for (var i = 0; i < path.node.args.length; i++) {
            buildArgs["$" + i] = path.node.args[i];
        }
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
    "transform-es2015-arrow-functions",
    "transform-es2015-classes",
    "transform-es2015-computed-properties",
    "transform-es2015-for-of",
    "transform-es2015-object-super",
    "transform-es2015-parameters",
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
        symbols: []
    }

    for (var i=2; i < process.argv.length; i+=2) {
        var key = process.argv[i].substring(2),
            value = process.argv[i+1];
        opts[key] = value;
    }
    
    var fabelCwd = process.cwd();
    var fabelCmd = process.platform === "win32" ? "cmd" : "mono";
    var fabelCmdArgs = [__dirname + "/../build/main/Fabel.exe"];
	if ( process.platform === "win32") {
	  fabelCmdArgs.unshift("/C");
	}

    if (typeof opts.projFile === "string") {
        fabelCwd = path.dirname(fabelCwd + "/" + opts.projFile);
        opts.projFile = path.basename(opts.projFile);
        
        var cfgFile = path.join(fabelCwd, "fabelconfig.json");
        if (fs.existsSync(cfgFile)) {
            var cfg = JSON.parse(fs.readFileSync(cfgFile).toString());
            for (var key in cfg) {
                opts[key] = cfg[key];
            }
        }
    }
    else if (typeof opts.code !== "string") {
        throw "No correct --projFile or --code argument provided";
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
    
    fabelCmdArgs.push(JSON.stringify(opts));
    console.log(fabelCmd + " " + fabelCmdArgs.join(" "));
    
    var proc = spawn(fabelCmd, fabelCmdArgs, { cwd: fabelCwd });

    proc.on('exit', function(code) {
        console.log("Finished with code " + code);
        process.exit(code);
    });    

    proc.stderr.on('data', function(data) {
        console.log("FABEL ERROR: " + data.toString().substring(0, 300) + "...");
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
        
        try {
            var babelAst = JSON.parse(json);
            if (opts.projFile) {
                babelifyToFile(fabelCwd, path.join(fabelCwd, opts.outDir), babelAst);
            }
            else {
                babelifyToConsole(babelAst);
            }
        }
        catch (err) {
            console.log("BABEL ERROR in " + babelAst.fileName + ": " + err);
            process.exit(1);
        }
    });    
}
catch (err) {
    console.log("ARG ERROR: " + err);
    process.exit(1);
}
