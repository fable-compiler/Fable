const fs = require("fs");
const Path = require("path");
const Babel = require("@babel/core");

const FSHARP_EXT = /\.(fs|fsx)$/i;

function ensureArray(obj) {
    return (Array.isArray(obj) ? obj : obj != null ? [obj] : []);
}

function getRelPath(fromPath, toPath) {
    let relPath = Path.relative(Path.dirname(fromPath), toPath);
    relPath = relPath.replace(/\\/g, "/").replace(FSHARP_EXT, "");
    relPath = relPath.endsWith(".js") ? relPath : relPath + ".js";
    return relPath.startsWith("..") ? relPath : "./" + relPath;
}

function fixImportPaths(babelAst, sourcePath) {
    const decls = ensureArray(babelAst.body);
    for (const decl of decls) {
        if (decl.source != null && typeof decl.source.value === "string") {
            const importPath = decl.source.value;
            if (importPath.startsWith("fable-core/") || importPath.match(FSHARP_EXT)) {
                decl.source.value = getRelPath(sourcePath, importPath);
            }
        }
    }
}

export function getVersion() {
    return require("../package.json").version;
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

export function transformAndSaveBabelAst(babelAst, fileName, outDir) {
    try {
        fixImportPaths(babelAst, fileName);
        const res = Babel.transformFromAstSync(babelAst);
        const jsPath = fileName.replace(FSHARP_EXT, ".js");
        const outPath = Path.join(outDir, jsPath);
        ensureDirExists(Path.dirname(outPath));
        fs.writeFileSync(outPath, res.code);
    } catch (err) {
        console.error(err);
    }
}
