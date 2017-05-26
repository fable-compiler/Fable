import * as Babel from "babel-core";
import * as fs from "fs";
import * as Path from "path";
import * as Process from "process";
const client = require("fable-utils/client");
const babelPlugins = require("fable-utils/babel-plugins");

const customPlugins = [
    babelPlugins.getRemoveUnneededNulls(),
    babelPlugins.getTransformMacroExpressions(Babel.template),
];

const DEFAULT_PORT = parseInt(Process.env.FABLE_SERVER_PORT || "61225", 10);
const FSHARP_EXT = /\.(fs|fsx|fsproj)$/;
const FSPROJ_EXT = /\.fsproj$/;
const JAVASCRIPT_EXT = /\.js$/;

let compiledPaths: Set<string>; // already compiled paths
let dedupOutPaths: Set<string>; // lookup of output paths
let mapInOutPaths: Map<string, string>; // map of input to output paths

export type FableOptions = {
    path?: string;
    define?: string[];
    plugins?: string[];
    fableCore?: string;
    declaration?: boolean;
    typedArrays?: boolean;
    clampByteArrays?: boolean;
    extra?: any;
};

export type FableCompilerOptions = {
    entry: string;
    outDir: string;
    port?: number;
    babel?: Babel.TransformOptions;
    fable?: FableOptions;
    prepack?: any;
};

function output(key: string) {
    switch (key) {
        case "warning": return console.warn;
        case "error": return console.error;
        default: return console.log;
    }
}

function ensureArray(obj: any) {
    return (Array.isArray(obj) ? obj : obj != null ? [obj] : []);
}

function ensureDirExists(dir: string) {
    if (dir && /(?:\/|\\)/.test(dir)) {
        ensureDirExists(Path.dirname(dir));
    }
    if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir);
    }
}

// TODO: implement better folder structure
function getOutPath(fullPath: string) {
    const srcPath = fullPath.replace(/\\/g, "/");
    let outPath = mapInOutPaths.get(srcPath);
    if (!outPath) {
        // get file name without extensions
        const fileName = Path.basename(srcPath)
            .replace(FSHARP_EXT, "").replace(JAVASCRIPT_EXT, "");
        // flat folder structure (one level deep)
        const fileDir = Path.basename(Path.dirname(srcPath));
        const newPath = Path.join(fileDir, fileName).replace(/\\/g, "/");
        // dedup output path
        let i = 0;
        outPath = newPath;
        while (dedupOutPaths.has(outPath)) {
            outPath = `${newPath}${++i}`;
        }
        dedupOutPaths.add(outPath);
        mapInOutPaths.set(srcPath, outPath);
    }
    return outPath;
}

function getFullPath(relPath: string) {
    const fullPath = Path.resolve(relPath).replace(/\\/g, "/");
    if (FSHARP_EXT.test(fullPath) || JAVASCRIPT_EXT.test(fullPath)) {
        return fullPath;
    } else {
        return fullPath + ".js";
    }
}

function fixPath(dir: string, path: string) {
    if (!path.startsWith(".")) { return path; } // no need to fix, i.e. node package
    const relPath = Path.join(dir, path);
    const fullPath = getFullPath(relPath);
    const newPath = Path.join("..", getOutPath(fullPath)); // assumes flat folder structure
    return newPath.replace(/\\/g, "/");
}

function getImportPaths(ast: any): string[] {
    const decls = ast && ast.program ? ensureArray(ast.program.body) : [];
    return decls
        .filter((d) => d.source != null)
        .map((d) => d.source.value);
}

function fixImportPaths(dir: string, ast: any) {
    const decls = ast && ast.program ? ensureArray(ast.program.body) : [];
    decls
        .filter((d) => d.source != null)
        .forEach((d) => { d.source.value = fixPath(dir, d.source.value); });
}

async function getFileAstAsync(path: string, options: FableCompilerOptions) {
    let result: Babel.BabelFileResult | undefined;
    if (FSHARP_EXT.test(path)) {
        // return Babel AST from F# file
        const fableMsg = JSON.stringify(Object.assign({}, options.fable, { path }));
        const babelAst = JSON.parse(await client.send(options.port, fableMsg));
        if (babelAst.error) { throw new Error(babelAst.error); }
        Object.keys(babelAst.logs || {}).forEach((key) => {
            ensureArray(babelAst.logs[key]).forEach(
                (log: string) => output(key)(`${key} ==> ${log}`));
        });
        result = Babel.transformFromAst(babelAst, undefined, { code: false });
    } else {
        // return Babel AST from JS file
        path = JAVASCRIPT_EXT.test(path) ? path : path + ".js";
        if (fs.existsSync(path)) {
            result = Babel.transformFileSync(path, { code: false });
        }
    }
    return result;
}

function transformAndSaveAst(fullPath: string, ast: any, options: FableCompilerOptions) {
    // resolve output paths
    const outPath = getOutPath(fullPath) + ".js";
    const jsPath = Path.join(options.outDir, outPath);
    const jsDir = Path.dirname(jsPath);
    ensureDirExists(jsDir);
    // set sourcemap paths
    const code: string | undefined = undefined;
    if (options.babel && options.babel.sourceMaps) {
        // code = fs.readFileSync(fullPath, "utf8");
        const relPath = Path.relative(jsDir, fullPath);
        options.babel.sourceFileName = relPath.replace(/\\/g, "/");
        options.babel.sourceMapTarget = Path.basename(outPath);
    }
    // transform and save
    let result = Babel.transformFromAst(ast, code, options.babel);
    if (options.prepack) {
        const prepack = require("prepack");
        result = prepack.prepackFromAst(result.ast, result.code, options.prepack);
    }
    if (result && result.code) { fs.writeFileSync(jsPath, result.code); }
    if (result && result.map) { fs.writeFileSync(jsPath + ".map", JSON.stringify(result.map)); }
    console.log(`Fable compiled: ${fullPath}`);
}

async function transformAsync(path: string, options: FableCompilerOptions) {
    // if already compiled, do nothing
    const fullPath = getFullPath(path);
    if (compiledPaths.has(fullPath)) {
        return;
    }
    compiledPaths.add(fullPath);

    // get file AST (no transformation)
    const result = await getFileAstAsync(path, options);
    if (result) {
        // get/fix import paths
        const importPaths = getImportPaths(result.ast);
        fixImportPaths(Path.dirname(path), result.ast);

        // if not a .fsproj, transform and save
        if (!FSPROJ_EXT.test(path)) {
            transformAndSaveAst(fullPath, result.ast, options);
        }

        // compile all dependencies (imports)
        const dir = Path.dirname(path);
        for (const importPath of importPaths) {
            const relPath = Path.join(dir, importPath);
            await transformAsync(relPath, options);
        }
    }
}

export default function fableSplitter(options: FableCompilerOptions) {
    // set defaults
    options = options || {};
    options.entry = options.entry || "";
    options.outDir = options.outDir || ".";
    options.port = options.port || DEFAULT_PORT;

    options.babel = options.babel || {};
    options.babel.plugins = customPlugins.concat(options.babel.plugins || []);

    options.fable = options.fable || {};
    // options.fable.path = options.fable.path;
    options.fable.define = options.fable.define || [];
    options.fable.plugins = options.fable.plugins || [];
    // options.fable.fableCore = options.fable.fableCore;
    options.fable.declaration = options.fable.declaration || false;
    options.fable.typedArrays = options.fable.typedArrays || true;
    options.fable.clampByteArrays = options.fable.clampByteArrays || false;
    // options.fable.extra = options.fable.extra;

    // options.prepack = options.prepack;

    // path lookups
    compiledPaths = new Set<string>();
    dedupOutPaths = new Set<string>();
    mapInOutPaths = new Map<string, string>();

    // main loop
    console.log("Fable compiler started ...");
    transformAsync(options.entry, options)
        .then(() => console.log("Fable compiler is done."))
        .catch((err) => {
            console.error(`ERROR: ${err.message}`);
            if (err.message.indexOf("ECONN") !== -1) {
                console.log(`Make sure Fable server is running on port ${options.port}`);
            }
        });
}
