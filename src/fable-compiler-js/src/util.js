const fs = require("fs");
const Path = require("path");
const Babel = require("@babel/core");
const BabelPlugins = require("fable-babel-plugins");

const customPlugins = [
    BabelPlugins.getRemoveUnneededNulls(),
    BabelPlugins.getTransformMacroExpressions(Babel.template)
];

const FSHARP_EXT = /\.(fs|fsx)$/i;
const JAVASCRIPT_EXT = /\.js$/i;

export function getVersion() {
    return require("../package.json").version;
}

export function serializeToJson(data) {
    return JSON.stringify(data, (key, value) => {
        if (value === Infinity) {
           return "Infinity";
        } else if (value === -Infinity) {
           return "-Infinity";
        } else if (value !== value) {
           return "NaN";
        }
        return value;
     });
}

export function ensureDirExists(dir, cont) {
    if (fs.existsSync(dir)) {
        if (typeof cont === "function") { cont(); }
    } else {
        ensureDirExists(Path.dirname(dir), () => {
            if (!fs.existsSync(dir)) { fs.mkdirSync(dir); }
            if (typeof cont === "function") { cont(); }
        });
    }
}

const uniquePaths = new Map();

function ensureUniquePath(sourcePath, outDir, relPath) {
    let outPath = Path.resolve(outDir, relPath);
    while (uniquePaths.has(outPath) && uniquePaths.get(outPath) !== sourcePath) {
        var i = outPath.lastIndexOf(".");
        outPath = (i < 0) ? outPath + "_" : outPath.substr(0, i) + "_" + outPath.substr(i);
    }
    if (!uniquePaths.has(outPath)) { uniquePaths.set(outPath, sourcePath); }
    return outPath;
}

function ensureArray(obj) {
    return (Array.isArray(obj) ? obj : obj != null ? [obj] : []);
}

function isRelativePath(path) {
    return path.startsWith("./") || path.startsWith("../");
}

function getRelPath(sourcePath, importPath, filePath, projDir, outDir) {
    if (isRelativePath(importPath)) {
        importPath = Path.resolve(Path.dirname(sourcePath), importPath);
    }
    let relPath = Path.relative(projDir, importPath).replace(/\\/g, "/");
    relPath = relPath.replace(/\.\.\//g, "").replace(/\.\//g, "").replace(/\:/g, "");
    relPath = Path.relative(Path.dirname(filePath), Path.join(outDir, relPath));
    relPath = relPath.replace(/\\/g, "/").replace(FSHARP_EXT, ".js");
    relPath = relPath.match(JAVASCRIPT_EXT) ? relPath : relPath + ".js";
    relPath = relPath.startsWith("..") ? relPath : "./" + relPath;
    return relPath;
}

function getJsImport(sourcePath, importPath, filePath, projDir, outDir, babelOptions) {
    const relPath = getRelPath(sourcePath, importPath, filePath, projDir, outDir);
    // transform and save javascript imports
    let jsPath = Path.resolve(Path.dirname(sourcePath), importPath);
    jsPath = jsPath.match(JAVASCRIPT_EXT) ? jsPath : jsPath + ".js";
    const resAst = Babel.transformFileSync(jsPath, { ast: true, code: false });
    const outPath = ensureUniquePath(jsPath, Path.dirname(filePath), relPath);
    fixImportPaths(resAst.ast, outPath, outDir);
    const resCode = Babel.transformFromAstSync(resAst.ast, null, babelOptions);
    ensureDirExists(Path.dirname(outPath));
    fs.writeFileSync(outPath, resCode.code);
    return relPath;
}

function fixImportPaths(babelAst, filePath, projDir, outDir, babelOptions) {
    const sourcePath = babelAst.fileName;
    const decls = ensureArray(babelAst.body);
    for (const decl of decls) {
        if (decl.source != null && typeof decl.source.value === "string") {
            const importPath = decl.source.value;
            if (importPath.startsWith("fable-library/")) {
                decl.source.value = getRelPath(filePath, Path.join(outDir, importPath), filePath, outDir, outDir);
            } else if (importPath.match(FSHARP_EXT)) {
                decl.source.value = getRelPath(sourcePath, importPath, filePath, projDir, outDir);
            } else if (isRelativePath(importPath) || Path.isAbsolute(importPath)) {
                decl.source.value = getJsImport(sourcePath, importPath, filePath, projDir, outDir, babelOptions);
            }
        }
    }
}

export function copyFolder(from, dest) {
    if (!fs.existsSync(dest)) {
        ensureDirExists(dest);
    }
    fs.readdirSync(from).forEach(element => {
        if (fs.lstatSync(Path.join(from, element)).isDirectory()) {
            copyFolder(Path.join(from, element), Path.join(dest, element));
        } else {
            fs.copyFileSync(Path.join(from, element), Path.join(dest, element));
        }
    });
}

export function transformAndSaveBabelAst(babelAst, filePath, projDir, outDir, commonjs) {
    try {
        // this solves a weird commonjs issue where some imports are not properly qualified
        babelAst = JSON.parse(serializeToJson(babelAst)); // somehow this helps with that
        const sourcePath = babelAst.fileName;
        const jsPath = filePath.replace(FSHARP_EXT, ".js");
        const outPath = ensureUniquePath(sourcePath, outDir, jsPath);
        ensureDirExists(Path.dirname(outPath));
        const babelOptions = commonjs ?
            { plugins: customPlugins.concat("@babel/plugin-transform-modules-commonjs") } :
            { plugins: customPlugins };
        fixImportPaths(babelAst, outPath, projDir, outDir, babelOptions);
        const res = Babel.transformFromAstSync(babelAst, null, babelOptions);
        fs.writeFileSync(outPath, res.code);
    } catch (err) {
        console.error(err);
    }
}

export function runCmdAndExitIfFails(cmd) {
    var child_process = require("child_process");
    console.log(">", cmd);
    try {
        child_process.execSync(cmd, {
            stdio: "inherit"
        });
    } catch (error) {
        process.exit(-1);
    }
}