import * as fs from "fs";
import * as Path from "path";
import Babel from "@babel/core";
import BabelPlugins from "fable-babel-plugins";

const customPlugins = [
  BabelPlugins.getRemoveUnneededNulls(),
  BabelPlugins.getTransformMacroExpressions(Babel.template)
];

const FSHARP_EXT = /\.(fs|fsx)$/i;
const JAVASCRIPT_EXT = /\.js$/i;

export function getVersion() {
  return require("../package.json").version;
}

export function getFableLibDir() {
  return Path.join(Path.dirname(require.resolve("fable-standalone")), "fable-library");
}

export function getDirFiles(dir) {
  if (!fs.existsSync(dir)) return [];
  const files = fs.readdirSync(dir).map((subdir) => {
    const res = Path.resolve(dir, subdir);
    return fs.statSync(res).isDirectory() ? getDirFiles(res) : res;
  });
  return files.reduce((a, f) => a.concat(f), []);
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

function ensureArray(obj) {
  return (Array.isArray(obj) ? obj : obj != null ? [obj] : []);
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

export function transformAndSaveBabelAst(babelAst, filePath, projDir, outDir, libDir, options) {
  try {
    // this solves a weird commonjs issue where some imports are not properly qualified
    babelAst = JSON.parse(serializeToJson(babelAst)); // somehow this helps with that
    const sourcePath = babelAst.fileName;
    const jsPath = filePath.replace(FSHARP_EXT, options.typeDecls ? ".ts" : ".js");
    let outPath = Path.resolve(outDir, jsPath);
    outPath = ensureUniquePath(sourcePath, outPath);
    ensureDirExists(Path.dirname(outPath));
    const babelOptions = options.commonjs ?
      { plugins: customPlugins.concat("@babel/plugin-transform-modules-commonjs") } :
      { plugins: customPlugins };
    if (options.sourceMaps) {
      babelOptions.sourceMaps = true;
    }
    fixImportPaths(babelAst, sourcePath, outPath, projDir, outDir, libDir, babelOptions);
    const babelOpts = Object.assign({}, babelOptions);
    if (babelOpts.sourceMaps) {
      const relPath = Path.relative(Path.dirname(outPath), sourcePath);
      babelOpts.sourceFileName = relPath.replace(/\\/g, "/");
    }
    const result = Babel.transformFromAstSync(babelAst, null, babelOpts);
    fs.writeFileSync(outPath, result.code);
    if (result.map) {
      fs.appendFileSync(outPath, "\n//# sourceMappingURL=" + Path.basename(outPath) + ".map");
      fs.writeFileSync(outPath + ".map", JSON.stringify(result.map));
  }
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