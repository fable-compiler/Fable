"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const Babel = require("babel-core");
const fableUtils = require("fable-utils");
const fs = require("fs");
const Path = require("path");
const Process = require("process");
const customPlugins = [
    fableUtils.babelPlugins.getRemoveUnneededNulls(),
    fableUtils.babelPlugins.getTransformMacroExpressions(Babel.template),
];
const DEFAULT_PORT = parseInt(Process.env.FABLE_SERVER_PORT || "61225", 10);
const FSHARP_EXT = /\.(fs|fsx|fsproj)$/;
const FSPROJ_EXT = /\.fsproj$/;
const JAVASCRIPT_EXT = /\.js$/;
const MACRO = /^\${(\w+)}[\\/]?(.*?)([\\/]?)$/;
function getResolvePathPlugin(targetDir, opts) {
    return {
        visitor: {
            StringLiteral(path) {
                const node = path.node;
                if (MACRO.test(node.value)) {
                    const match = MACRO.exec(node.value) || [];
                    let replacement = opts.outDir;
                    if (match[1] === "entryDir") {
                        replacement = Path.dirname(opts.entry);
                    }
                    else if (match[1] !== "outDir") {
                        throw new Error("Unknown macro: " + node.value);
                    }
                    const fullPath = Path.join(replacement, match[2]);
                    const newRelPath = (Path.relative(targetDir, fullPath) + match[3]).replace(/\\/g, "/");
                    // console.log("FULL PATH: " + fullPath);
                    // console.log("REL. PATH: " + newRelPath);
                    node.value = newRelPath;
                }
            },
        },
    };
}
function output(msg, severity) {
    if (severity === "warning") {
        console.warn(msg);
    }
    else if (severity === "error") {
        console.error(msg);
    }
    else {
        console.log(msg);
    }
}
function addLogs(logs, info) {
    if (typeof logs === "object") {
        Object.keys(logs).forEach((key) => {
            info.logs[key] = key in info.logs
                ? info.logs[key].concat(logs[key])
                : ensureArray(logs[key]);
        });
    }
}
function ensureArray(obj) {
    return (Array.isArray(obj) ? obj : obj != null ? [obj] : []);
}
function ensureDirExists(dir, cont) {
    if (fs.existsSync(dir)) {
        if (typeof cont === "function") {
            cont();
        }
    }
    else {
        ensureDirExists(Path.dirname(dir), () => {
            if (!fs.existsSync(dir)) {
                fs.mkdirSync(dir);
            }
            if (typeof cont === "function") {
                cont();
            }
        });
    }
}
exports.ensureDirExists = ensureDirExists;
// TODO: implement better folder structure
function getOutPath(path, info) {
    let outPath = info.mapInOutPaths.get(path);
    if (!outPath) {
        // get file name without extensions
        const fileName = Path.basename(path).replace(FSHARP_EXT, "").replace(JAVASCRIPT_EXT, "");
        // flat folder structure (one level deep)
        const pathDir = Path.dirname(path);
        // If pathDir is same as entry dir don't create nested folder
        const newPath = pathDir === Path.dirname(info.entry)
            ? fileName : Path.basename(pathDir) + "/" + fileName;
        // dedup output path
        let i = 0;
        outPath = newPath;
        while (info.dedupOutPaths.has(outPath)) {
            outPath = `${newPath}.${++i}`;
        }
        info.dedupOutPaths.add(outPath);
        info.mapInOutPaths.set(path, outPath);
    }
    return outPath;
}
function getFullPath(relPath, isDir) {
    const fullPath = Path.resolve(relPath).replace(/\\/g, "/");
    if (isDir || FSHARP_EXT.test(fullPath) || JAVASCRIPT_EXT.test(fullPath)) {
        return fullPath;
    }
    else {
        return fullPath + ".js";
    }
}
/** Joins two paths if second is not absolute and normalizes slashes */
function join(path1, path2) {
    const path = Path.isAbsolute(path2) ? path2 : Path.join(path1, path2);
    return path.replace(/\\/g, "/");
}
/** Fix the import declaration to match the output file structure */
function fixImportPath(fromDir, path, info) {
    const outPath = getOutPath(getFullPath(join(fromDir, path)), info);
    const isNested = outPath.indexOf("/") >= 0;
    const fromEntryDir = fromDir === Path.dirname(info.entry);
    // Assumes flat folder structure
    if (isNested && fromEntryDir) {
        return "./" + outPath;
    }
    else if (isNested) {
        return Path.basename(fromDir) === Path.dirname(outPath)
            ? "./" + Path.basename(outPath)
            : "../" + outPath;
    }
    else {
        return (fromEntryDir ? "./" : "../") + outPath;
    }
}
/** Ignores paths to external modules like "react/react-dom-server" */
function getRelativeOrAbsoluteImportDeclarations(ast) {
    const decls = ast && ast.program ? ensureArray(ast.program.body) : [];
    return decls.filter((d) => {
        if (d.source != null && typeof d.source.value === "string") {
            const path = d.source.value;
            return path.startsWith(".") || Path.isAbsolute(path);
        }
        return false;
    });
}
function getFileAstAsync(path, options, info) {
    return __awaiter(this, void 0, void 0, function* () {
        let ast;
        if (FSHARP_EXT.test(path)) {
            // return Babel AST from F# file
            const fableMsg = JSON.stringify(Object.assign({}, options.fable, { path }));
            const response = yield fableUtils.client.send(options.port, fableMsg);
            const babelAst = JSON.parse(response);
            if (babelAst.error) {
                throw new Error(babelAst.error);
            }
            addLogs(babelAst.logs, info);
            ast = Babel.transformFromAst(babelAst, undefined, { code: false });
        }
        else {
            // return Babel AST from JS file
            path = JAVASCRIPT_EXT.test(path) ? path : path + ".js";
            if (fs.existsSync(path)) {
                try {
                    ast = Babel.transformFileSync(path, { code: false });
                }
                catch (err) {
                    const log = `${path}(1,1): error BABEL: ${err.message}`;
                    addLogs({ error: [log] }, info);
                }
            }
            else {
                console.log(`fable: Skipping missing JS file: ${path}`);
            }
        }
        return ast;
    });
}
function transformAndSaveAst(fullPath, ast, options, info) {
    // resolve output paths
    const outPath = getOutPath(fullPath, info) + ".js";
    const jsPath = join(options.outDir, outPath);
    const jsDir = Path.dirname(jsPath);
    ensureDirExists(jsDir);
    // set sourcemap paths
    const code = undefined;
    const babelOptions = Object.assign({}, options.babel);
    if (babelOptions.sourceMaps) {
        // code = fs.readFileSync(fullPath, "utf8");
        const relPath = Path.relative(jsDir, fullPath);
        babelOptions.sourceFileName = relPath.replace(/\\/g, "/");
        babelOptions.sourceMapTarget = Path.basename(outPath);
    }
    babelOptions.plugins = (babelOptions.plugins || [])
        .concat(getResolvePathPlugin(jsDir, options));
    // transform and save
    let result = Babel.transformFromAst(ast, code, babelOptions);
    if (options.prepack) {
        const prepack = require("prepack");
        result = prepack.prepackFromAst(result.ast, result.code, options.prepack);
    }
    fs.writeFileSync(jsPath, result.code);
    if (result.map) {
        fs.appendFileSync(jsPath, "\n//# sourceMappingURL=" + Path.basename(jsPath) + ".map");
        fs.writeFileSync(jsPath + ".map", JSON.stringify(result.map));
    }
    console.log(`fable: Compiled ${Path.relative(process.cwd(), fullPath)}`);
}
function transformAsync(path, options, info, force) {
    return __awaiter(this, void 0, void 0, function* () {
        const fullPath = getFullPath(path);
        if (!info.compiledPaths.has(fullPath)) {
            info.compiledPaths.add(fullPath);
        }
        else if (!force) {
            return;
        }
        // get file AST (no transformation)
        const ast = yield getFileAstAsync(fullPath, options, info);
        if (ast) {
            const importPaths = [];
            const fromDir = Path.dirname(fullPath);
            for (const decl of getRelativeOrAbsoluteImportDeclarations(ast.ast)) {
                const importPath = decl.source.value;
                importPaths.push(importPath);
                decl.source.value = fixImportPath(fromDir, importPath, info);
            }
            // if not a .fsproj, transform and save
            if (!FSPROJ_EXT.test(fullPath)) {
                transformAndSaveAst(fullPath, ast.ast, options, info);
            }
            // compile all dependencies (imports)
            const dir = Path.dirname(fullPath);
            for (const importPath of importPaths) {
                const relPath = join(dir, importPath);
                yield transformAsync(relPath, options, info);
            }
        }
    });
}
function setDefaultOptions(options) {
    options = Object.assign({}, options);
    options.entry = getFullPath(options.entry); // Normalize path
    options.outDir = getFullPath(options.outDir || ".", true);
    options.port = options.port || DEFAULT_PORT;
    options.fable = options.fable || {};
    options.babel = options.babel || {};
    options.babel.plugins = customPlugins.concat(options.babel.plugins || []);
    // options.prepack = options.prepack;
    return options;
}
function createCompilationInfo(options, previousInfo) {
    if (previousInfo == null) {
        return {
            entry: options.entry,
            compiledPaths: new Set(),
            dedupOutPaths: new Set(),
            mapInOutPaths: new Map(),
            logs: {},
        };
    }
    else {
        return {
            entry: options.entry,
            compiledPaths: new Set(previousInfo.compiledPaths),
            dedupOutPaths: new Set(previousInfo.dedupOutPaths),
            mapInOutPaths: new Map(previousInfo.mapInOutPaths),
            logs: {},
        };
    }
}

function msToTime(s) {
    var ms = s % 1000;
    s = (s - ms) / 1000;
    var secs = s % 60;
    s = (s - secs) / 60;
    var mins = s % 60;
    var hrs = (s - mins) / 60;
  
    return hrs + ':' + mins + ':' + secs + '.' + ms;
  }

function fableSplitter(options, previousInfo) {
    fableUtils.validateFableOptions(options);
    options = setDefaultOptions(options);
    const info = createCompilationInfo(options, previousInfo);
    // main loop
    const startDate = new Date();
    const startDateStr = startDate.toLocaleTimeString();
    console.log(`fable: Compilation started at ${startDateStr}`);
    // options.path will only be filled in watch compilations
    return transformAsync(options.path || options.entry, options, info, true)
        .then(() => {
        Object.keys(info.logs).forEach((severity) => ensureArray(info.logs[severity]).forEach((log) => output(log, severity)));
        const hasError = Array.isArray(info.logs.error) && info.logs.error.length > 0;
        const date = new Date();
        const dateStr = date.toLocaleTimeString();
        const duration = msToTime(date - startDate);
        console.log(`fable: Compilation ${hasError ? "failed" : "succeeded"} at ${dateStr} (${duration})`);
        if (!hasError && typeof options.postbuild === "function") {
            options.postbuild();
        }
        return info;
    })
        .catch((err) => {
        console.error(`ERROR: ${err.message}`);
        if (err.message.indexOf("ECONN") !== -1) {
            console.log(`Make sure Fable server is running on port ${options.port}`);
        }
        return info;
    });
}
exports.default = fableSplitter;
