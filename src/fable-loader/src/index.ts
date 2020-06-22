/// @ts-check

const DEFAULT_COMPILER = "fable-compiler"
// const DEFAULT_COMPILER = require("../fable-compiler"); // testing

import * as fs from "fs";
import * as path from "path";
import * as babel from "@babel/core";
import * as babelPlugins from "fable-babel-plugins";
import tcpClient from "./tcpClient";

let firstCompilationFinished = false;
const customPlugins: babel.PluginItem[] = [
    babelPlugins.getTransformMacroExpressions(babel.template)
];

interface Options {
    cli?: any;
    extra?: any;
    babel?: babel.TransformOptions;
    define?: string[]|string;
    typedArrays?: boolean;
    clampByteArrays?: boolean;
    compiler?: string,
    port?: number,
    silent?: boolean,
    cache?: boolean,
    watch?: boolean,
}

interface Compiler {
    compile(req: CompilationRequest): Promise<CompilationResult>,
}

interface CompilationRequest {
    path: string,
    rootDir: string,
    define: string[],
    typedArrays: boolean,
    clampByteArrays: boolean,
    extra: any
}

interface CompilationResult {
    fileName: string,
    error?: string,
    dependencies?: string[],
    logs: { [key: string]: string[] }
}

interface WebpackHelper {
    addDependency(s: string): void
    emitError(s: string): void,
    emitWarning(s: string): void,
    sourceMap?: boolean,
    cacheable(b: boolean): void,
    buffer: Buffer,
    onCompiled(f: ()=>void): void
}

type Option<T> = T | undefined;
const None = undefined;

interface FileCache {
    getFile(path: string): Promise<Option<string>>,
    setFile(path: string, content?: string): void
}

function getTcpPort(opts: Options): Option<number> {
    if (opts.port != null) {
        return opts.port;
    } else if (process.env.FABLE_SERVER_PORT != null) {
        return parseInt(process.env.FABLE_SERVER_PORT, 10);
    } else {
        return undefined;
    }
}

const getCompiler = (function() {
    let compiler: Option<Compiler> = undefined;
    return function(opts: Options, webpack: WebpackHelper): Compiler {
        if (compiler == null) {
            const port = getTcpPort(opts);
            if (port != null) {
                compiler = {
                    compile(req) {
                        return tcpClient("127.0.0.1", port, JSON.stringify(req))
                            .then(json => JSON.parse(json));
                    },
                }
            } else {
                const fableCompiler = require(opts.compiler ?? DEFAULT_COMPILER).default(opts.cli);
                webpack.onCompiled(function() {
                    firstCompilationFinished = true;
                    if (!opts.watch) {
                        fableCompiler.close();
                    }
                });
                compiler = {
                    compile(req) {
                        return fableCompiler.send(req);
                    }
                }
            }
        }
        return compiler;
    }
}());

const getFileCache = (function() {
    let fileCache: Option<FileCache> = undefined;
    return async function(options: Options): Promise<FileCache> {
        if (options.watch && options.cache) {
            if (fileCache == null) {
                const hash = stringHash(JSON.stringify({
                    client: "fable-loader",
                    compilerVersion: require("fable-compiler/package.json").version,
                    ...options
                })).toString(16);

                const cacheDir = path.join(process.cwd(), ".fable", "cache", hash);
                await createDirIfNotExists(cacheDir, () =>
                    tryWriteFile(path.join(process.cwd(), ".fable", ".gitignore"), "**/*"));

                function toCachePath(filePath: string) {
                    filePath = path.resolve(filePath); // Normalize
                    const hash = stringHash(filePath).toString(16);
                    return path.join(cacheDir, hash + ".js");
                }

                fileCache = {
                    // Cache is only active during first watch compilation
                    getFile(path: string) {
                        return !path.endsWith(".fsproj") && !firstCompilationFinished
                            ? tryReadFile(toCachePath(path))
                            : Promise.resolve(None);
                    },
                    // Don't return the promise so we don't block compilation
                    setFile(path: string, content?: string) {
                        tryWriteFile(toCachePath(path), content);
                    }
                };
            }
            return fileCache
        } else {
            return {
                getFile(_path) { return Promise.resolve(None) },
                setFile(_path, _content) { }
            }
        }
    }
}())

function transformBabelAst(babelAst, babelOptions, sourceMapOptions): Promise<babel.BabelFileResult> {
    let fsCode = undefined;
    if (sourceMapOptions != null) {
        fsCode = sourceMapOptions.buffer.toString();
        babelOptions.sourceMaps = true;
        const fileName = sourceMapOptions.path.replace(/\\/g, '/');
        babelOptions.filename = fileName;
        babelOptions.sourceFileName = path.relative(process.cwd(), fileName);
    }
    return new Promise(function(resolve, reject) {
        babel.transformFromAst(babelAst, fsCode, babelOptions, function (err, result) {
            err ? reject(err) : resolve(result ?? {});
        });
    });
}

async function compile(filePath: string, opts: Options, webpack: WebpackHelper) {
    const fileCache = await getFileCache(opts);
    const cachedFile = await fileCache.getFile(filePath);
    if (cachedFile != null) {
        log(opts, "fable: Cached " + path.relative(process.cwd(), filePath));
        return { code: cachedFile }
    }

    const req: CompilationRequest = {
        path: filePath,
        rootDir: process.cwd(),
        define: ensureArray(opts.define),
        typedArrays: opts.typedArrays ?? false,
        clampByteArrays: opts.clampByteArrays ?? false,
        extra: opts.extra ?? {}
    };
    const compiler = getCompiler(opts, webpack);
    const data = await compiler.compile(req);
    if (data.error) {
        throw new Error(data.error);
    }

    if (!req.path.endsWith(".fsproj")) {
        ensureArray(data.dependencies).forEach(p => {
            // Fable normalizes path separator to '/' which causes issues in Windows
            // Use `path.resolve` to restore the separator to the system default
            webpack.addDependency(path.resolve(p));
        });
    }

    let isErrored = false;
    if (typeof data.logs === "object") {
        Object.keys(data.logs).forEach(key => {
            ensureArray(data.logs[key]).forEach(msg => {
                switch (key)  {
                    case "error":
                        isErrored = true;
                        webpack.emitError(msg);
                        break;
                    case "warning":
                        webpack.emitWarning(msg);
                        break;
                    default:
                        log(opts, msg)
                }
            });
        });
    }
    webpack.cacheable(!isErrored);

    const babelParsed = await transformBabelAst(data, opts.babel, webpack.sourceMap ? {
        path: data.fileName,
        buffer: webpack.buffer
    } : null);

    fileCache.setFile(req.path, babelParsed.code ?? undefined);
    log(opts, "fable: Compiled " + path.relative(process.cwd(), req.path));
    return babelParsed;
}

function Loader(buffer: Buffer) {
    const callback = this.async();
    const webpackCompiler = this._compiler;

    const opts: Options = this.loaders[0].options ?? {};
    opts.cli = opts.cli ?? {};
    opts.cli.silent = opts.silent;
    opts.babel = opts.babel ?? {};
    opts.babel.plugins = customPlugins.concat(opts.babel.plugins ?? []);
    opts.extra = opts.extra ?? {}
    opts.watch = webpackCompiler.watchMode;
    opts.define = ensureArray(opts.define);
    if (webpackCompiler?.options?.mode === "development" && opts.define.indexOf("DEBUG") === -1) {
        opts.define.push("DEBUG");
    }

    const webpackHelper: WebpackHelper = {
        addDependency: (s: string) => this.addDependency(s),
        emitError: (s: string) => this.emitError(new Error(s)),
        emitWarning: (s: string) => this.emitWarning(new Error(s)),
        sourceMap: this.sourceMap,
        cacheable: (b: boolean) => this.cacheable(b),
        buffer: buffer,
        onCompiled(f) {
            webpackCompiler.hooks.done.tap("fable-loader", f);
        }
    }

    compile(this.resourcePath, opts, webpackHelper).then(
        result => callback(null, result.code, result.map),
        err => callback(err)
    );
};

Loader.raw = true;
export default Loader;

// Helpers

function ensureArray<T>(x: Option<T|T[]>) {
    return x == null ? [] : Array.isArray(x) ? x : [x];
}

function log(opts: Options, msg: string) {
    if (!opts.silent) {
        console.log(msg);
    }
}

function createDirIfNotExists(dirPath: string, onCreation?: ()=>void): Promise<void> {
    return fs.promises.access(dirPath, fs.constants.F_OK).catch(_err =>
        fs.promises.mkdir(dirPath, { recursive: true }).then(() => {
            if (onCreation != null) {
                onCreation();
            }
        }));
}

function tryReadFile(filePath: string): Promise<Option<string>> {
    return fs.promises.readFile(filePath).then(b => b.toString(), _err => None);
}

function tryWriteFile(filePath: string, content?: string): void {
    if (content != null) {
        fs.promises.writeFile(filePath, content)
            .catch(function (_err) {}); // Ignore errors
    }
}

function stringHash(str: string) {
    let i = 0;
    let h = 5381;
    const len = str.length;
    while (i < len) {
      h = (h * 33) ^ str.charCodeAt(i++);
    }
    return h;
  }