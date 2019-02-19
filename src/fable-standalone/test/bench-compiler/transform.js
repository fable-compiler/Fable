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

function ensureArray(obj) {
    return (Array.isArray(obj) ? obj : obj != null ? [obj] : []);
}

function ensureDirExists(dir, cont) {
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

function getFilePaths(dir) {
    const subdirs = fs.readdirSync(dir);
    const files = subdirs.map((subdir) => {
        const res = Path.resolve(dir, subdir);
        return fs.statSync(res).isDirectory() ? getFilePaths(res) : res;
    });
    return files.reduce((acc, file) => acc.concat(file), []);
}

function main() {
    const projPath = Path.resolve(process.argv[2]);
    const projDir = Path.dirname(projPath)
    const outDir = Path.resolve(process.argv[3]);
    const commonjs = process.argv.find(v => v === "--commonjs");
    const babelOptions = commonjs ?
        { plugins: customPlugins.concat("@babel/plugin-transform-modules-commonjs") } :
        { plugins: customPlugins };

    console.log("Compiling to " + (commonjs ? "commonjs" : "ES2015 modules") + "...")

    const filePaths = getFilePaths(outDir);
    for (const filePath of filePaths) {
        if (filePath.endsWith(".json")) {
            const babelJson = fs.readFileSync(filePath, "utf8");
            const babelAst = JSON.parse(babelJson);
            fixImportPaths(babelAst, filePath, projDir, outDir, babelOptions);
            const res = Babel.transformFromAstSync(babelAst, null, babelOptions);
            const fileOut = filePath.replace(/\.json$/, ".js")
            fs.renameSync(filePath, fileOut);
            fs.writeFileSync(fileOut, res.code);
        }
    }
}

main()
