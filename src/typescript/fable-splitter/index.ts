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

let alreadyCompiled: Set<string>;
let dedupFlatPaths: Set<string>;

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

// flat folder structure (one level deep)
function flatPath(fullPath: string) {
    // file name without extensions
    const fileName = Path.basename(fullPath)
        .replace(FSHARP_EXT, "")
        .replace(JAVASCRIPT_EXT, "");
    const fileDir = Path.basename(Path.dirname(fullPath));
    const filePath = Path.join(fileDir, fileName);
    // dedup new path
    let newPath = filePath;
    let i = 0;
    while (dedupFlatPaths.has(newPath)) {
        newPath = `${filePath}${++i}`;
    }
    return newPath;
}

function fixPath(dir: string, path: string) {
    if (!path.startsWith(".")) { return path; } // node package
    const relPath = Path.join(dir, path);
    const fullPath = Path.resolve(relPath);
    const newPath = Path.join("..", flatPath(fullPath));
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

function transformAst(ast: any, babelOptions?: Babel.TransformOptions) {
    const code: string | undefined = undefined;
    if (babelOptions && babelOptions.sourceMaps) {
        babelOptions.sourceFileName = Path.relative(Process.cwd(), ast.fileName.replace(/\\/g, "/"));
    }
    const result = Babel.transformFromAst(ast, code, babelOptions);
    return result;
}

// TODO: better folder structure
function saveToFile(fullPath: any, code: string, options: FableCompilerOptions) {
    console.log(`Fable compiled: ${fullPath}`);
    const jsPath = Path.join(options.outDir, flatPath(fullPath) + ".js");
    ensureDirExists(Path.dirname(jsPath));
    fs.writeFileSync(jsPath, code);
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

function getFullPath(relPath: string) {
    const fullPath = Path.resolve(relPath).replace(/\\/g, "/");
    if (FSHARP_EXT.test(fullPath) || JAVASCRIPT_EXT.test(fullPath)) {
        return fullPath;
    } else {
        return fullPath + ".js";
    }
}

async function transformAsync(path: string, options: FableCompilerOptions) {
    // if already compiled, do nothing
    const fullPath = getFullPath(path);
    if (alreadyCompiled.has(fullPath)) {
        return;
    }
    alreadyCompiled.add(fullPath);

    // get file AST (no transformation)
    const result = await getFileAstAsync(path, options);
    if (result) {
        // get/fix import paths
        const importPaths = getImportPaths(result.ast);
        fixImportPaths(Path.dirname(path), result.ast);

        // if not a .fsproj, transform and save
        if (!FSPROJ_EXT.test(path)) {
            const transformed = transformAst(result.ast, options.babel);
            saveToFile(fullPath, transformed.code || "", options);
        }

        // compile all dependencies (imports)
        const dir = Path.dirname(path);
        for (const importPath of importPaths) {
            const relPath = Path.join(dir, importPath);
            await transformAsync(relPath, options);
        }
    }
}

export default function fableCompiler(options: FableCompilerOptions) {
    options = options || {};

    const babelOptions = options.babel || {};
    babelOptions.plugins = customPlugins.concat(babelOptions.plugins || []);

    const fableOptions = options.fable || {};
    fableOptions.path = fableOptions.path;
    fableOptions.define = fableOptions.define || [];
    fableOptions.plugins = fableOptions.plugins || [];
    fableOptions.fableCore = fableOptions.fableCore;
    fableOptions.declaration = fableOptions.declaration || false;
    fableOptions.typedArrays = fableOptions.typedArrays || true;
    fableOptions.clampByteArrays = fableOptions.clampByteArrays || false;
    fableOptions.extra = fableOptions.extra;

    options = {
        entry: options.entry || "",
        outDir: options.outDir || ".",
        port: options.port || DEFAULT_PORT,
        babel: babelOptions,
        fable: fableOptions,
    };

    alreadyCompiled = new Set<string>();
    dedupFlatPaths = new Set<string>();

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
