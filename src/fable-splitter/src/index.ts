import * as Babel from "@babel/core";
import chalk from "chalk";
import * as babelPlugins from "fable-babel-plugins";
import * as fs from "fs-extra";
import * as Path from "path";
import getCompiler from "./compiler";
import * as NetClient from "./net-client";

function getTcpPort(opts: FableSplitterOptions) {
    if (opts.port != null) {
        return opts.port;
    } else if (process.env.FABLE_SERVER_PORT != null) {
        return parseInt(process.env.FABLE_SERVER_PORT, 10);
    } else {
        return null;
    }
}

const FSHARP_EXT = /\.(fs|fsx|fsproj)$/;
const FSPROJ_EXT = /\.fsproj$/;
const JAVASCRIPT_EXT = /\.js$/;
const MACRO = /^\${(\w+)}[\\/]?(.*?)([\\/]?)$/;

const customPlugins: any[] = [
    babelPlugins.getRemoveUnneededNulls(),
    babelPlugins.getTransformMacroExpressions(Babel.template),
];

export type CompilationInfo = {
    entry: string,
    projectFiles: string[],
    compiledPaths: Set<string>, // already compiled paths
    dedupOutPaths: Set<string>, // lookup of output paths
    mapInOutPaths: Map<string, string>, // map of input to output paths
    logs: { [severity: string]: string[] },
};

export type FableOptions = {
    define?: string[],
    plugins?: string[],
    typedArrays?: boolean,
    clampByteArrays?: boolean,
    // extra?: any,
};

export type FableSplitterOptions = {
    entry: string,
    outDir: string,
    path?: string,
    babel?: Babel.TransformOptions,
    fable?: FableOptions,
    port?: number
    cli?: {},
    allFiles?: boolean,
    externals?: any,
    postbuild?: () => void,
};

function getResolvePathPlugin(targetDir: string, opts: FableSplitterOptions) {
    return {
        visitor: {
            StringLiteral(path: any) {
                const node = path.node;
                if (MACRO.test(node.value)) {
                    const match = MACRO.exec(node.value) || [];
                    let replacement: string = opts.outDir;
                    if (match[1] === "entryDir") {
                        replacement = Path.dirname(opts.entry);
                    } else if (match[1] !== "outDir") {
                        throw new Error("Unknown macro: " + node.value);
                    }
                    const fullPath = Path.join(replacement, match[2]);
                    const newRelPath = (Path.relative(targetDir, fullPath) + match[3]).replace(/\\/g, "/");
                    // console.log("FULL PATH: " + fullPath);
                    // console.log("REL. PATH: " + newRelPath);
                    node.value = isRelativePath(newRelPath) ? newRelPath : "./" + newRelPath;
                }
            },
        },
    };
}

function output(msg: string, severity: string) {
    if (severity === "warning") {
        console.warn(chalk.bold.yellow(msg));
    } else if (severity === "error") {
        console.error(chalk.bold.red(msg));
    } else {
        console.log(msg);
    }
}

function addLogs(logs: { [key: string]: string[] }, info: CompilationInfo) {
    if (typeof logs === "object") {
        Object.keys(logs).forEach((key) => {
            info.logs[key] = key in info.logs
                ? info.logs[key].concat(logs[key])
                : ensureArray(logs[key]);
        });
    }
}

function ensureArray(obj: any) {
    return (Array.isArray(obj) ? obj : obj != null ? [obj] : []);
}

export function ensureDirExists(dir: string, cont?: () => void) {
    if (fs.existsSync(dir)) {
        if (typeof cont === "function") { cont(); }
    } else {
        ensureDirExists(Path.dirname(dir), () => {
            if (!fs.existsSync(dir)) { fs.mkdirSync(dir); }
            if (typeof cont === "function") { cont(); }
        });
    }
}

// TODO: implement better folder structure
function getOutPath(path: string, info: CompilationInfo): string {
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
        // In Windows and Mac file paths are case insensitive
        // so it may happen we get two identical paths with different case
        while (info.dedupOutPaths.has(outPath.toLowerCase())) {
            outPath = `${newPath}.${++i}`;
        }
        info.dedupOutPaths.add(outPath.toLowerCase());
        info.mapInOutPaths.set(path, outPath);
    }
    return outPath;
}

function getFullPath(relPath: string, isDir?: boolean) {
    const fullPath = Path.resolve(relPath).replace(/\\/g, "/");
    if (isDir || FSHARP_EXT.test(fullPath) || JAVASCRIPT_EXT.test(fullPath)) {
        return fullPath;
    } else {
        return fullPath + ".js";
    }
}

/** Joins two paths if second is not absolute and normalizes slashes */
function join(path1: string, path2: string) {
    const path = Path.isAbsolute(path2) ? path2 : Path.join(path1, path2);
    return path.replace(/\\/g, "/");
}

/** Fix the import declaration to match the output file structure */
function fixImportPath(fromDir: string, path: string, info: CompilationInfo) {
    const outPath = getOutPath(getFullPath(join(fromDir, path)), info);
    const isNested = outPath.indexOf("/") >= 0;
    const fromEntryDir = fromDir === Path.dirname(info.entry);
    // Assumes flat folder structure
    if (isNested && fromEntryDir) {
        return "./" + outPath;
    } else if (isNested) {
        return Path.basename(fromDir) === Path.dirname(outPath)
            ? "./" + Path.basename(outPath)
            : "../" + outPath;
    } else {
        return (fromEntryDir ? "./" : "../") + outPath;
    }
}

function isRelativePath(path: string) {
    return path.startsWith("./") || path.startsWith("../");
}

/** Ignores paths to external modules like "react/react-dom-server" */
function getRelativeOrAbsoluteImportDeclarations(ast: Babel.types.Program) {
    const decls = ensureArray(ast.body);
    return decls.filter((d) => {
        if (d.source != null && typeof d.source.value === "string") {
            const path = d.source.value;
            return isRelativePath(path) || Path.isAbsolute(path);
        }
        return false;
    });
}

function varDeclarator(ident: string, init: any) {
    return {
        type: "VariableDeclarator",
        id: {
            type: "Identifier",
            name: ident,
        },
        init,
    };
}

function member(left: string, right: string) {
    return {
        type: "MemberExpression",
        object: {
          type: "Identifier",
          name: left,
        },
        property: {
            type: "Identifier",
            name: right,
        },
        computed: false,
    };
}

function fixExternalImports(ast: Babel.types.Program, externals: any) {
    if (Array.isArray(ast.body)) {
        const importDecls: any[] = [];
        const fixedDecls: any[] = [];
        const otherDecls = ast.body.filter((decl: any) => {
            if (decl.source != null && typeof decl.source.value === "string") {
                const path: string = decl.source.value;
                if (path in externals) {
                    const replacement = externals[path];
                    // TODO: Check for ImportNamespaceSpecifier
                    const varDeclarators = decl.specifiers.map((specifier) =>
                        varDeclarator(specifier.local.name, member(replacement, specifier.imported.name)));
                    fixedDecls.push({
                        type: "VariableDeclaration",
                        declarations: varDeclarators,
                        kind: "const",
                    });
                } else {
                    importDecls.push(decl);
                }
                return false;
            }
            return true;
        });
        ast.body = importDecls.concat(fixedDecls, otherDecls);
    }
}

async function getBabelAst(path: string, options: FableSplitterOptions, info: CompilationInfo) {
    let ast: Babel.types.Program | null = null;
    if (FSHARP_EXT.test(path)) {
        // return Babel AST from F# file
        const msg = Object.assign({},
            options.fable,
            { path, rootDir: process.cwd() },
        );
        const port = getTcpPort(options);
        const babelAst = port != null
            ? await NetClient.send("127.0.0.1", port, msg)
            : await getCompiler(options.cli).send(msg);
        if (babelAst.error) {
            throw new Error(babelAst.error);
        } else if (path.endsWith(".fsproj")) {
            info.projectFiles = babelAst.sourceFiles;
        }
        addLogs(babelAst.logs, info);
        ast = babelAst;
    } else {
        // return Babel AST from JS file
        path = JAVASCRIPT_EXT.test(path) ? path : path + ".js";
        if (fs.existsSync(path)) {
            ast = await getBabelAstFromJsFile(path, info);
        } else {
            console.log(`fable: Skip missing JS file: ${path}`);
        }
    }
    return ast;
}

function getBabelAstFromJsFile(path: string, info: CompilationInfo) {
    return new Promise<Babel.types.Program | null>((resolve) => {
        const babelOpts = {
            code: false,
            ast: true,
            // Prevent Babel from searching for config files, see #1751
            configFile: false,
            babelrc: false
        };
        Babel.transformFile(path, babelOpts, (error, res) => {
            if (error != null) {
                const log = `${path}(1,1): error BABEL: ${error.message}`;
                addLogs({ error: [log] }, info);
                resolve(null);
            } else {
                let program: Babel.types.Program | null = null;
                if (res != null && res.ast != null) {
                    program = (res.ast as Babel.types.File).program;
                }
                resolve(program);
            }
        });
    });
}

function generateJsCodeFromBabelAst(ast: Babel.types.Program, code?: string,
                                    options?: Babel.TransformOptions) {
    return new Promise<Babel.BabelFileResult | null>((resolve) => {
        Babel.transformFromAst(ast, code, options, (error, res) => {
            if (error != null) {
                console.error("fable: Error transforming Babel AST", error);
                resolve(null);
            } else {
                resolve(res);
            }
        });
    });
}

async function generateJsCode(fullPath: string, ast: Babel.types.Program,
                              options: FableSplitterOptions,
                              info: CompilationInfo) {
    // resolve output paths
    const outPath = getOutPath(fullPath, info) + ".js";
    const jsPath = join(options.outDir, outPath);
    const jsDir = Path.dirname(jsPath);
    ensureDirExists(jsDir);

    // set sourcemap paths
    const code: string | undefined = undefined;
    const babelOptions = Object.assign({}, options.babel) as Babel.TransformOptions;
    if (babelOptions.sourceMaps) {
        // code = fs.readFileSync(fullPath, "utf8");
        const relPath = Path.relative(jsDir, fullPath);
        babelOptions.sourceFileName = relPath.replace(/\\/g, "/");
    }

    // Add ResolvePathPlugin
    babelOptions.plugins = (babelOptions.plugins || [])
        .concat(getResolvePathPlugin(jsDir, options));

    // transform and save
    const result = await generateJsCodeFromBabelAst(ast, code, babelOptions);
    if (result != null) {
        await fs.writeFile(jsPath, result.code);
        if (result.map) {
            await fs.appendFile(jsPath, "\n//# sourceMappingURL=" + Path.basename(jsPath) + ".map");
            await fs.writeFile(jsPath + ".map", JSON.stringify(result.map));
        }
        console.log(`fable: Compiled ${Path.relative(process.cwd(), fullPath)}`);
    }
}

async function transformAsync(path: string, options: FableSplitterOptions,
                              info: CompilationInfo, force?: boolean): Promise<void> {
    const fullPath = getFullPath(path);
    if (!info.compiledPaths.has(fullPath)) {
        info.compiledPaths.add(fullPath);
    } else if (!force) {
        return;
    }

    const ast = await getBabelAst(fullPath, options, info);

    if (ast != null) {
        const importPaths: string[] = [];
        const fromDir = Path.dirname(fullPath);

        // Fix import paths
        for (const decl of getRelativeOrAbsoluteImportDeclarations(ast)) {
            const importPath = decl.source.value;
            importPaths.push(importPath);
            decl.source.value = fixImportPath(fromDir, importPath, info);
        }

        if (options.externals != null) {
            fixExternalImports(ast, options.externals);
        }

        // if not an .fsproj, transform and save
        if (!FSPROJ_EXT.test(fullPath)) {
            await generateJsCode(fullPath, ast, options, info);
        }

        // compile all dependencies (imports)
        const dir = Path.dirname(fullPath);
        for (const importPath of importPaths) {
            const relPath = join(dir, importPath);
            await transformAsync(relPath, options, info);
        }
    }
}

function setDefaultOptions(options: FableSplitterOptions) {
    if (options.entry == null || options.entry.length === 0) {
        throw new Error("Missing or empty: entry path");
    }
    options = Object.assign({}, options);
    options.entry = getFullPath(options.entry); // Normalize path
    options.outDir = getFullPath(options.outDir || ".", true);

    options.fable = options.fable || {};
    options.babel = options.babel || {};
    options.babel.plugins = customPlugins.concat(options.babel.plugins || []);
    return options;
}

function createCompilationInfo(options: FableSplitterOptions, previousInfo?: CompilationInfo): CompilationInfo {
    if (previousInfo == null) {
        return {
            entry: options.entry,
            projectFiles: [],
            compiledPaths: new Set<string>(),
            dedupOutPaths: new Set<string>(),
            mapInOutPaths: new Map<string, string>(),
            logs: {} as { [key: string]: string[] },
        };
    } else {
        return {
            entry: options.entry,
            projectFiles: previousInfo.projectFiles,
            compiledPaths: new Set<string>(previousInfo.compiledPaths),
            dedupOutPaths: new Set<string>(previousInfo.dedupOutPaths),
            mapInOutPaths: new Map<string, string>(previousInfo.mapInOutPaths),
            logs: {} as { [key: string]: string[] },
        };
    }
}

export default function fableSplitter(options: FableSplitterOptions, previousInfo?: CompilationInfo) {
    options = setDefaultOptions(options);
    const info = createCompilationInfo(options, previousInfo);

    // main loop
    const startDate = new Date();
    const startDateStr = startDate.toLocaleTimeString();
    console.log(`fable: Compilation started at ${startDateStr}`);

    const startTime = process.hrtime();

    // options.path will only be filled in watch compilations
    return transformAsync(options.path || options.entry, options, info, true)
        .then(() => {
            if (options.allFiles) {
                const promises: Array<Promise<void>> = [];
                for (const file of ensureArray(info.projectFiles)) {
                    promises.push(transformAsync(file, options, info));
                }
                return Promise.all(promises) as Promise<any>;
            } else {
                return Promise.resolve();
            }
        })
        .then(() => {
            Object.keys(info.logs).forEach((severity) =>
                ensureArray(info.logs[severity]).forEach((log) =>
                    output(log, severity)));

            const hasError = Array.isArray(info.logs.error) && info.logs.error.length > 0;
            const date = new Date();
            const dateStr = date.toLocaleTimeString();
            const elapsed = process.hrtime(startTime);
            const duration = (elapsed[0] + elapsed[1] / 1e9).toFixed(3);

            console.log(`fable: Compilation ${hasError ? "failed" : "succeeded"} at ${dateStr} (${duration} s)`);

            if (!hasError && typeof options.postbuild === "function") {
                options.postbuild();
            }
            return info;
        })
        .catch((err) => {
            console.error(`ERROR: ${err.message}`);
            return info;
        });
}
