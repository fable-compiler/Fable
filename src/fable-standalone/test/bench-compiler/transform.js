const fs = require("fs");
const Path = require("path");
const Babel = require("@babel/core");
const BabelPlugins = require("fable-babel-plugins");

const FSHARP_EXT = /\.(fs|fsx)$/i;
const JAVASCRIPT_EXT = /\.js$/i;

function ensureArray(obj) {
    return (Array.isArray(obj) ? obj : obj != null ? [obj] : []);
}

function getRelPath(fromPath, toPath) {
    let relPath = Path.relative(Path.dirname(fromPath), toPath);
    relPath = relPath.replace(/\\/g, "/").replace(FSHARP_EXT, "");
    relPath = relPath.match(JAVASCRIPT_EXT) ? relPath : relPath + ".js";
    return relPath.startsWith("..") ? relPath : "./" + relPath;
}

function fixImportPaths(babelAst, filePath, outDir) {
    const sourcePath = Path.relative(outDir, filePath);
    const decls = ensureArray(babelAst.body);
    for (const decl of decls) {
        if (decl.source != null && typeof decl.source.value === "string") {
            const importPath = decl.source.value;
            if (importPath.startsWith("fable-library/") ||
                importPath.match(FSHARP_EXT) ||
                importPath.match(JAVASCRIPT_EXT)) {
                decl.source.value = getRelPath(sourcePath, importPath);
            }
        }
    }
}

const useCommonjs = process.argv.find(v => v === "--commonjs");
console.log("Compiling to " + (useCommonjs ? "commonjs" : "ES2015 modules") + "...")

const customPlugins = [
    BabelPlugins.getRemoveUnneededNulls(),
    BabelPlugins.getTransformMacroExpressions(Babel.template)
];

const babelOptions = {
    plugins: useCommonjs ? customPlugins.concat("@babel/plugin-transform-modules-commonjs") : customPlugins,
};

function getFilePaths(dir) {
    const subdirs = fs.readdirSync(dir);
    const files = subdirs.map((subdir) => {
        const res = Path.resolve(dir, subdir);
        return fs.statSync(res).isDirectory() ? getFilePaths(res) : res;
    });
    return files.reduce((acc, file) => acc.concat(file), []);
}

const outDir = process.argv[2];
const filePaths = getFilePaths(outDir);

for (const filePath of filePaths) {
    if (filePath.endsWith(".json")) {
        const babelJson = fs.readFileSync(filePath, "utf8");
        const babelAst = JSON.parse(babelJson);
        fixImportPaths(babelAst, filePath, outDir);
        const res = Babel.transformFromAstSync(babelAst, null, babelOptions);
        const fileOut = filePath.replace(/\.json$/, ".js")
        fs.renameSync(filePath, fileOut);
        fs.writeFileSync(fileOut, res.code);
    }
}