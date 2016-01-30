/* global __dirname */
/* global process */

var fs = require("fs");
var path = require("path");
var babel = require("babel-core");
var template = require("babel-template");
var exec = require('child_process').exec;

// Custom plugin to simulate macro expressions
var transformMacroExpressions = {
  visitor: {
    TemplateLiteral(path) {
      if (!path.node.macro)
          return;
  
      var macro = "";
      var quasisLength = path.node.quasis.length;
      for (var i = 0; i < quasisLength; i++) {
          macro += path.node.quasis[i].value.raw;
          if (i < quasisLength - 1) {
              macro += "$" + i;
          }
      }
      
      var buildArgs = {};
      var buildMacro = template(macro);
  
      for (var i = 0; i < path.node.expressions.length; i++) {
          buildArgs["$" + i] = path.node.expressions[i];
      }
      
      path.replaceWith(buildMacro(buildArgs));
    }
  }
};

var babelPlugins = [
    transformMacroExpressions,
    "transform-do-expressions",
    "transform-es2015-arrow-functions",
    "transform-es2015-classes",
    "transform-es2015-computed-properties",
    "transform-es2015-for-of",
    "transform-es2015-spread",
    "transform-es2015-modules-umd"
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

function babelifyToFile(projDir, outDir, babelAstArray) {
    for (var i=0; i<babelAstArray.length; i++) {
        var babelAst = babelAstArray[i];
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
    console.log("Complete");
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
    var fabelCmd = process.platform === "win32" ? "cmd " : "mono ";
    fabelCmd += __dirname + "/../build/main/Fabel.exe ";

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
    fabelCmd += "'" + JSON.stringify(opts).replace("'", "''") + "'";
    console.log(fabelCmd);
    
    exec(fabelCmd, { cwd: fabelCwd }, function(error, stdout, stderr) {
        if (error != null) {
            var errorMessage = typeof stdout === "string" && stdout.length > 0
                                ? stdout : stderr; 
            console.log("FABEL ERROR: " + errorMessage);
            process.exit(1);
        }
        else {
            try {
                var babelAstArray = JSON.parse(stdout);
                if (opts.projFile) {
                    babelifyToFile(fabelCwd, path.join(fabelCwd, opts.outDir), babelAstArray);
                }
                else {
                    babelifyToConsole(babelAstArray[0]);
                }
                process.exit(0);
            }
            catch (err) {
                console.log("BABEL ERROR: " + err);
                process.exit(1);
            }
        }
    });
}
catch (err) {
    console.log("ARG ERROR: " + err);
    process.exit(1);
}
