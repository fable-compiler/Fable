import * as fs from "fs";
import * as Path from "path";
import Babel from "@babel/core";
import BabelPlugins from "fable-babel-plugins";

const customPlugins = [
  BabelPlugins.getRemoveUnneededNulls(),
  BabelPlugins.getTransformMacroExpressions(Babel.template),
  // "babel-plugin-closure-elimination"
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

function ensureUniquePath(sourcePath, outPath) {
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

function getRelPath(sourcePath, importPath, outPath, projDir, outDir) {
  if (isRelativePath(importPath)) {
    importPath = Path.resolve(Path.dirname(sourcePath), importPath);
  }
  let relPath = Path.relative(projDir, importPath).replace(/\\/g, "/");
  relPath = relPath.replace(/\.\.\//g, "").replace(/\.\//g, "").replace(/\:/g, "");
  relPath = Path.relative(Path.dirname(outPath), Path.join(outDir, relPath));
  relPath = relPath.replace(/\\/g, "/").replace(FSHARP_EXT, ".js");
  relPath = relPath.match(JAVASCRIPT_EXT) ? relPath : relPath + ".js";
  relPath = relPath.startsWith("..") ? relPath : "./" + relPath;
  return relPath;
}

function getJsImport(sourcePath, importPath, outPath, projDir, outDir, babelOptions) {
  const relPath = getRelPath(sourcePath, importPath, outPath, projDir, outDir);
  let jsPath = isRelativePath(importPath) ?
    Path.resolve(Path.dirname(sourcePath), importPath) : importPath;
  jsPath = jsPath.match(JAVASCRIPT_EXT) ? jsPath : jsPath + ".js";
  outPath = Path.resolve(Path.dirname(outPath), relPath);
  // if not already done, transform and save javascript imports
  if (!uniquePaths.has(outPath) || uniquePaths.get(outPath) !== jsPath) {
    outPath = ensureUniquePath(jsPath, outPath);
    const resAst = Babel.transformFileSync(jsPath, { ast: true, code: false });
    fixImportPaths(resAst.ast.program, jsPath, outPath, projDir, outDir, outDir, babelOptions);
    const resCode = Babel.transformFromAstSync(resAst.ast, null, babelOptions);
    ensureDirExists(Path.dirname(outPath));
    fs.writeFileSync(outPath, resCode.code);
  }
  return relPath;
}

function fixImportPaths(babelAst, sourcePath, outPath, projDir, outDir, libDir, babelOptions) {
  const decls = ensureArray(babelAst.body);
  for (const decl of decls) {
    if (decl.source != null && typeof decl.source.value === "string") {
      let importPath = decl.source.value;
      if (importPath.startsWith("fable-library/")) {
        importPath = Path.join(libDir, importPath.replace(/^fable-library\//, ""));
        decl.source.value = getJsImport(sourcePath, importPath, outPath, Path.dirname(libDir), outDir, babelOptions);
      } else if (importPath.match(FSHARP_EXT)) {
        decl.source.value = getRelPath(sourcePath, importPath, outPath, projDir, outDir);
      } else if (isRelativePath(importPath) || Path.isAbsolute(importPath)) {
        decl.source.value = getJsImport(sourcePath, importPath, outPath, projDir, outDir, babelOptions);
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
  if (process.argv.length < 5) {
    console.log("Usage: node transform <projectPath> <out-dir> <fable-library-dir> [--commonjs]");
    return;
  }
  const projPath = Path.resolve(process.argv[2]);
  const projDir = Path.dirname(projPath)
  const outDir = Path.resolve(process.argv[3]);
  const libDir = Path.resolve(process.argv[4]);
  const commonjs = process.argv.find(v => v === "--commonjs");
  const sourceMaps = process.argv.find(v => v === "--sourceMaps");
  const typeDecls = process.argv.find(v => v === "--typed");

  const babelOptions = commonjs ?
    { plugins: customPlugins.concat("@babel/plugin-transform-modules-commonjs") } :
    { plugins: customPlugins };
  if (sourceMaps) { babelOptions.sourceMaps = true; }

  console.log("Compiling to " + (commonjs ? "commonjs" : "ES2015 modules") + "...")

  const filePaths = getFilePaths(outDir);
  for (const filePath of filePaths) {
    if (filePath.endsWith(".json")) {
      const babelJson = fs.readFileSync(filePath, "utf8");
      const babelAst = JSON.parse(babelJson);
      const sourcePath = babelAst.fileName;
      const outPath = filePath.replace(/\.json$/, typeDecls ? ".ts" : ".js");
      fixImportPaths(babelAst, sourcePath, outPath, projDir, outDir, libDir, babelOptions);
      const babelOpts = Object.assign({}, babelOptions);
      if (babelOpts.sourceMaps) {
        const relPath = Path.relative(Path.dirname(outPath), sourcePath);
        babelOpts.sourceFileName = relPath.replace(/\\/g, "/");
      }
      const result = Babel.transformFromAstSync(babelAst, null, babelOpts);
      fs.renameSync(filePath, outPath);
      fs.writeFileSync(outPath, result.code);
      if (result.map) {
        fs.appendFileSync(outPath, "\n//# sourceMappingURL=" + Path.basename(outPath) + ".map");
        fs.writeFileSync(outPath + ".map", JSON.stringify(result.map));
      }
    }
  }

  // fix import paths from /fable-library/ to /
  if (projPath.endsWith("Fable.Library.fsproj")) {
    const filePaths = getFilePaths(outDir);
    for (const filePath of filePaths) {
      if (filePath.endsWith(".ts")) {
        const code = fs.readFileSync(filePath, "utf8");
        const replaced = code.replace(/(?<=import .* from "\.+)\/fable-library\//g, "/");
        fs.writeFileSync(filePath, replaced);
      }
    }
  }
}

main()
