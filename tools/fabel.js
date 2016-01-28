var fs = require("fs");
var babel = require("babel-core");
var exec = require('child_process').exec;
var transformMacroExpressions = require("./babel-plugin-transform-macro-expressions.js");

var babelPlugins = [
    transformMacroExpressions,
    "transform-do-expressions",
    "transform-es2015-modules-commonjs"
];

function babelify(babelAst) {
    var opts = { plugins: babelPlugins };
    var parsed = babel.transformFromAst(babelAst, null, opts);
    return parsed.code;
}

function babelifyToFile(sourceFile, babelAst) {
    var fileNameNoExt = sourceFile.replace(/\.\w+$/, "");
    var targetFile = fileNameNoExt + ".js", sourceMapFile = fileNameNoExt + ".js.map";
    
    var opts = {
        sourceMaps: true,
        sourceMapTarget: targetFile,
        sourceFileName: sourceFile,
        plugins: babelPlugins
    };

    var parsed = babel.transformFromAst(babelAst, null, opts);
    fs.writeFileSync(targetFile, parsed.code + "\n//# sourceMappingURL=" + sourceMapFile);
    fs.writeFileSync(sourceMapFile, JSON.stringify(parsed.map));
}

var sourceFile = null;
var fabelCmd = 'mono ../build/Fabel.exe ';
if (process.argv[2] == "--file") {
    sourceFile = process.argv[3];
    fabelCmd += "--file " + sourceFile;
}
else {
    fabelCmd += "'" + process.argv[2] + "'";
}

exec(fabelCmd, function(error, stdout, stderr) {
    if (error != null) {
        console.log(stderr);
    }
    else {
        var babelAst = JSON.parse(stdout);
        if (sourceFile) {
            babelifyToFile(sourceFile, babelAst);
            console.log("Complete");
        }
        else {
            console.log(babelify(babelAst));
        }
    }
});

