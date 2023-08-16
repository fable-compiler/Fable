import * as fs from "fs";
import * as Path from "path";
import Babel from "@babel/core";
import BabelPlugins from "fable-babel-plugins";

const customPlugins = [
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

function getRelPath(absPath, outPath, projDir, outDir) {
  let relPath = Path.relative(projDir, absPath).replace(/\\/g, "/");
  relPath = relPath.replace(/\.\.\//g, "").replace(/\.\//g, "").replace(/\:/g, "");
  relPath = Path.relative(Path.dirname(outPath), Path.join(outDir, relPath));
  relPath = relPath.replace(/\\/g, "/");
  relPath = relPath.startsWith("..") ? relPath : "./" + relPath;
  return relPath;
}

function getJsImport(sourcePath, importPath, outPath, projDir, outDir, libDir, babelOptions, options) {
  if (importPath.startsWith("fable-library/")) {
    importPath = Path.join(libDir, importPath.replace(/^fable-library\//, ""));
    projDir = Path.dirname(libDir);
  }
  let relPath = importPath;
  let absPath = isRelativePath(importPath) ?
    Path.resolve(Path.dirname(sourcePath), importPath) : importPath;
  if (Path.isAbsolute(absPath)) {
    relPath = getRelPath(absPath, outPath, projDir, outDir);
    if (importPath.match(FSHARP_EXT)) {
      relPath = relPath.replace(FSHARP_EXT, options.typescript ? "" : ".js");
    } else {
      relPath = (relPath.match(JAVASCRIPT_EXT) || options.typescript) ? relPath : relPath + ".js";
      absPath = absPath.match(JAVASCRIPT_EXT) ? absPath : absPath + ".js";
      outPath = Path.resolve(Path.dirname(outPath), relPath);
      outPath = outPath.match(JAVASCRIPT_EXT) ? outPath : outPath + ".js";
      // if not already done, transform and save javascript imports
      if (!uniquePaths.has(outPath) || uniquePaths.get(outPath) !== absPath) {
        outPath = ensureUniquePath(absPath, outPath);
        ensureDirExists(Path.dirname(outPath));
        const resAst = Babel.transformFileSync(absPath, { ast: true, code: false });
        fixImportPaths(resAst.ast.program, absPath, outPath, projDir, outDir, libDir, babelOptions, options);
        const resCode = Babel.transformFromAstSync(resAst.ast, null, babelOptions);
        fs.writeFileSync(outPath, resCode.code);
      }
    }
  }
  return relPath;
}

function fixImportPaths(babelAst, sourcePath, outPath, projDir, outDir, libDir, babelOptions, options) {
  const decls = ensureArray(babelAst.body);
  for (const decl of decls) {
    if (decl.source != null && typeof decl.source.value === "string") {
      const importPath = decl.source.value;
      decl.source.value = getJsImport(sourcePath, importPath, outPath, projDir, outDir, libDir, babelOptions, options);
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
  const options = {
    commonjs: process.argv.find(v => v === "--commonjs") != null,
    sourceMaps: process.argv.find(v => v === "--sourceMaps") != null,
    classTypes: process.argv.find(v => v === "--classTypes") != null,
  };

  const babelOptions = options.commonjs ?
    { plugins: customPlugins.concat("@babel/plugin-transform-modules-commonjs") } :
    { plugins: customPlugins };
  if (options.sourceMaps) {
    babelOptions.sourceMaps = true;
  }

  console.log("Compiling to " + (options.commonjs ? "commonjs" : "ES2015 modules") + "...")

  const filePaths = getFilePaths(outDir);
  for (const filePath of filePaths) {
    if (filePath.endsWith(".json")) {
      const babelJson = fs.readFileSync(filePath, "utf8");
      const babelAst = JSON.parse(babelJson);
      const sourcePath = babelAst.fileName;
      const outPath = filePath.replace(/\.json$/, options.typescript ? ".ts" : ".js");
      fixImportPaths(babelAst, sourcePath, outPath, projDir, outDir, libDir, babelOptions, options);
      const babelOpts = Object.assign({}, babelOptions);
      if (babelOpts.sourceMaps) {
        const relPath = Path.relative(Path.dirname(outPath), sourcePath);
        babelOpts.sourceFileName = relPath.replace(/\\/g, "/");
      }
      const result = Babel.transformFromAstSync(babelAst, null, babelOpts);
      const source = options.typescript ?
        result.code.replace(/\$INTERFACE_DECL_PREFIX\$_/g, "") : result.code;
      fs.renameSync(filePath, outPath);
      fs.writeFileSync(outPath, source);
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
