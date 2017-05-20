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

const DEFAULT_PORT = parseInt(process.env.FABLE_SERVER_PORT || "61225", 10);
const FSHARP_EXT = /\.(fs|fsx|fsproj)$/;
const FSPROJ_EXT = /\.fsproj$/;
const JAVASCRIPT_EXT = /\.js$/;

let alreadyCompiled: Set<string>;

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

// function getCommonBaseDir(filePaths: string[]) {
//     function getCommonPrefix(parts: string[][]): string[] {
//         function f(prefix: string[], xs: string[][]): string[] {
//             if (xs.length === 0) {
//                 return prefix;
//             } else {
//                 const x = xs[0];
//                 let i = 0;
//                 while (i < prefix.length && i < x.length && x[i] === prefix[i]) {
//                     i = i + 1;
//                 }
//                 return f(prefix.slice(0, i), xs.slice(1));
//             }
//         }
//         return parts.length === 0 ? [] : f(parts[0], parts.slice(1));
//     }
//     const normalized = filePaths.map((filePath) => Path.dirname(filePath).replace(/\\/g, "/").split("/"));
//     return getCommonPrefix(normalized).join("/");
// }

function compileAST(babelAst: string, babelOptions?: babel.TransformOptions) {
    const data = JSON.parse(babelAst);
    if (data.error) { throw new Error(data.error); }

    Object.keys(data.logs || {}).forEach((key) => {
        const out = output(key);
        ensureArray(data.logs[key]).forEach((log: string) => out(`${key} ==> ${log}`));
    });

    const code: string | undefined = undefined;
    if (babelOptions && babelOptions.sourceMaps) {
        babelOptions.sourceFileName = Path.relative(Process.cwd(), data.fileName.replace(/\\/g, "/"));
    }
    const result = Babel.transformFromAst(data, code, babelOptions);
    return result;
}

async function transformAsync(path: string, options: FableCompilerOptions) {
    // if already compiled, do nothing
    let fullPath = Path.resolve(path).replace(/\\/g, "/");
    if (alreadyCompiled.has(fullPath)) {
        return undefined;
    }
    alreadyCompiled.add(fullPath);

    let parsed: Babel.BabelFileResult | undefined;

    // compile F#
    if (FSHARP_EXT.test(path)) {
        const fableMsg = JSON.stringify(Object.assign({}, options.fable, { path }));
        const babelAst = await client.send(options.port, fableMsg);
        parsed = compileAST(babelAst, options.babel);
    } else {
        // compile JS
        fullPath = JAVASCRIPT_EXT.test(fullPath) ? fullPath : fullPath + ".js";
        if (fs.existsSync(fullPath)) {
            parsed = Babel.transformFileSync(fullPath, options.babel);
        }
    }

    if (parsed) {
        console.log(`Fable compiled: ${fullPath}`);

        // if not a .fsproj, save as JS file
        if (!FSPROJ_EXT.test(path)) {
            // TODO: create common folder structure
            const fileName = Path.basename(fullPath).replace(FSHARP_EXT, ".js");
            const filePath = Path.resolve(Path.join(options.outDir, fileName));
            const fileDir = Path.dirname(filePath);
            if (!fs.existsSync(fileDir)) { fs.mkdirSync(fileDir); }
            fs.writeFileSync(filePath, parsed.code);
        }
    }

    return parsed;
}

async function transformLoopAsync(path: string, options: FableCompilerOptions) {
    const result: any = await transformAsync(path, options);
    if (result && result.ast && result.ast.program) {
        const dir = Path.dirname(path);
        const files = result.ast.program.body || [];
        for (const file of files) {
            if (file.source) {
                const filePath = Path.join(dir, file.source.value);
                await transformLoopAsync(filePath, options);
            }
        }
    }
}

export default function fableCompiler(options: FableCompilerOptions) {
    options = options || {};

    const babelOptions = options.babel || {};
    babelOptions.plugins = customPlugins.concat(babelOptions.plugins || []);

    const fableOptions = options.fable || {};
    fableOptions.path = fableOptions.path || "";
    fableOptions.define = fableOptions.define || [];
    fableOptions.plugins = fableOptions.plugins || [];
    fableOptions.fableCore = fableOptions.fableCore || "../fable-core";
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

    // main loop
    transformLoopAsync(options.entry, options)
        .then(() => console.log("Fable compiler is done."))
        .catch((err) => console.error(err.message));
}
