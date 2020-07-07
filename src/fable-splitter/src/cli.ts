#!/usr/bin/env node

import * as fs from "fs";
import * as path from "path";
import fableSplitter, { CompilationInfo, FableSplitterOptions, RunOptions } from "./index";
import runScript from "./run";

const calledFromCommandLine = require.main === module;

function requireLazy(module: string) {
    let cache = null;
    return () => cache == null ? cache = require(module) : cache;
}

function getMainScriptPath(options: FableSplitterOptions, info: CompilationInfo) {

    function getFromFsproj(projectInfo: CompilationInfo) {
        const lastFile = projectInfo.projectFiles[projectInfo.projectFiles.length - 1];
        return projectInfo.mapInOutPaths.get(lastFile);
    }

    function getFromScript(scriptInfo: CompilationInfo) {
        return scriptInfo.mapInOutPaths.get(scriptInfo.entry);
    }

    const mainScriptName =
        info.projectFiles.length > 0 ? getFromFsproj(info) : getFromScript(info);

    const mainScript = path.join(options.outDir, mainScriptName || "");
    return mainScript.endsWith(".js") ? mainScript : mainScript + ".js";
}

function getVersion() {
    /// @ts-ignore
    return require("../package.json").version;
}

function getHelp() {
    return `fable-splitter ${getVersion()}
Usage: fable-splitter [command] [arguments]

Commands:
  -h|--help         Show help
  --version         Print version
  [file/dir path]   Compile an F# project to JS

Arguments:
  -o|--outDir       Output directory
  -c|--config       Config file
  -w|--watch        [FLAG] Watch mode
  -d|--debug        [FLAG] Define DEBUG constant
  --allFiles        [FLAG] Compile all files in the F# project
  --commonjs        [FLAG] Compile to commonjs modules
  --usePolling      [FLAG] Option for watch mode, may help capture file
                    save events in certain editors (suboptimal)
  --run             [FLAG] Run script with node after compilation
                    Arguments after --run will be passed to the script
  --runInspect      [FLAG] Like run, but passes --inspect option to node
                    so a debugger can be attached

Examples:
  fable-splitter src/App.fsproj -o dist/
  fable-splitter src/ -w --run
`;
}

function findFlag(arr: string[], ...flags: string[]): true|undefined {
    return findFlagIndex(arr, ...flags) > -1 ? true : undefined;
}

// TODO: Stop after --run?
function findFlagIndex(arr: string[], ...flags: string[]): number {
    for (let i = 0; i < arr.length; i++) {
        if (flags.indexOf(arr[i]) >= 0) {
            return i;
        }
    }
    return -1;
}

// TODO: Stop after --run?
function findArgValue(arr: string[], ...args: string[]): string|undefined {
    for (let i = 0; i < arr.length; i++) {
        if (args.indexOf(arr[i]) >= 0) {
            return arr[i + 1];
        }
    }
    return undefined;
}

function buildOptionsFromCliArgs(args: string[]): FableSplitterOptions {
    let entry = "";
    let firstArg = args[0];
    if (firstArg && !firstArg.startsWith("-")) {
        entry = path.resolve(firstArg);
        args = args.slice(1);
    }

    let run: RunOptions|undefined = undefined;
    let runIndex = findFlagIndex(args, "--run");
    if (runIndex >= 0) {
        run = { args: args.slice(runIndex + 1) };
    } else {
        runIndex = findFlagIndex(args, "--runInspect");
        if (runIndex >= 0) {
            run = {
                inspect: true,
                args: args.slice(runIndex + 1)
            };
        }
    }

    return {
        entry,
        outDir: findArgValue(args, "-o", "--outDir") || "",
        allFiles: findFlag(args, "--allFiles"),
        debug: findFlag(args, "-d", "--debug"),
        run,
        commonjs: findFlag(args, "--commonjs"),
        watch: findFlag(args, "-w", "--watch"),
        usePolling: findFlag(args, "--usePolling")
    };
}

function tooClose(filename: string, prev?: [string, Date]): boolean {
    return prev != null &&
        filename === prev[0] &&
        new Date().getTime() - prev[1].getTime() < 2000;
}

/** Like Object.assign but checks first if properties in the source are null or empty strings */
function assignNonNullOrEmptyString(target: object, source: object) {
    for (const k in source) {
        if (source[k] != null && source[k] !== "") {
            target[k] = source[k];
        }
    }
    return target;
}

function concatSafe<T>(ar: T[]|undefined, item: T): T[] {
    if (ar == null) {
        return [item];
    } else {
        ar = Array.isArray(ar) ? ar : [ar];
        if (ar.indexOf(item) === -1) {
            return ar.concat(item);
        } else {
            return ar;
        }
    }
}

/** Reads config from first match in filePaths argument */
function tryReadConfig(...filePaths: string[]): FableSplitterOptions|undefined {
    for (let filePath of filePaths) {
        filePath = path.resolve(filePath);
        if (fs.existsSync(filePath)) {
            return require(filePath);
        }
    }
    return undefined;
}

function forceReadConfig(filePath: string): FableSplitterOptions {
    const cfg = tryReadConfig(filePath);
    if (cfg == null) {
        throw new Error("Cannot read config file: " + filePath);
    }
    return cfg;
}

function fixEntryPath(entry: string|null) {
    entry = entry || path.resolve(".");
    if (!/\.fs(x|proj)$/.test(entry)) {
        const candidates = fs.readdirSync(entry).filter((x) => x.endsWith(".fsproj"));
        if (candidates.length === 0) {
            throw new Error(`Cannot find an .fsproj file in ${entry}`);
        } else if (candidates.length > 1) {
            throw new Error(`Found more than one .fsproj file in ${entry}. Disambiguate.`);
        } else {
            return path.join(entry, candidates[0]);
        }
    }
    return entry;
}

function onCompiled(opts: FableSplitterOptions, info: CompilationInfo, mustFinish = false) {
    const hasError = Array.isArray(info.logs.error) && info.logs.error.length > 0;
    if (!hasError) {
        if (typeof opts.onCompiled === "function") {
            opts.onCompiled();
        }
        if (opts.run) {
            const runOpts = typeof opts.run === "boolean" ? { args: [] } : opts.run;
            const job = runScript(getMainScriptPath(opts, info), runOpts);
            if (mustFinish) {
                job.then((code) => { process.exit(code); });
                return;
            }
        }
    } else {
        if (typeof opts.onErrored === "function") {
            opts.onErrored();
        }
    }

    if (mustFinish) {
        process.exit(hasError ? 1 : 0);
    }

}

export default function run(entryOpts: FableSplitterOptions, cfgFile?: string) {
    // Try read options from config file
    const opts: FableSplitterOptions = cfgFile
        ? forceReadConfig(cfgFile)
        : (tryReadConfig("fable-splitter.config.js", "splitter.config.js") || {
            entry: "",
            outDir: ""
        });

    // Merge options from config file with entry options and fix values
    assignNonNullOrEmptyString(opts, entryOpts);
    opts.entry = fixEntryPath(opts.entry);
    opts.outDir = opts.outDir ? path.resolve(opts.outDir) : path.join(path.dirname(opts.entry), "bin");
    if (opts.debug) {
        const fableOpts = opts.fable || {};
        fableOpts.define = concatSafe(fableOpts.define, "DEBUG");
        opts.fable = fableOpts;
    }
    // If we're passing --run assume we're compiling for Node
    // TODO: In the future, we may want to use ES2015 modules and .mjs files
    if (opts.commonjs || opts.run) {
        const babelOpts = opts.babel || {};
        babelOpts.plugins = concatSafe(babelOpts.define, "@babel/plugin-transform-modules-commonjs");
        opts.babel = babelOpts;
    }

    console.log(`fable-splitter ${getVersion()}`);
    const chokidarLazy = requireLazy("chokidar");

    fableSplitter(opts).then((info) => {
        if (opts.watch) {
            onCompiled(opts, info);
            let cachedInfo = info;
            let ready = false;
            let next: [string, Date]|undefined = undefined;
            let prev: [string, Date]|undefined = undefined;
            const watcher = chokidarLazy()
                .watch(Array.from(info.compiledPaths), {
                    ignored: /node_modules/,
                    persistent: true,
                    usePolling: opts.usePolling
                })
                .on("ready", () => {
                    console.log("fable: Watching...");
                    ready = true;
                })
                .on("all", (ev: string, filePath: string) => {
                    if (ready && ev === "change") {
                        prev = next;
                        next = [filePath, new Date()];
                        if (!tooClose(filePath, prev)) {
                            // console.log(ev + ": " + filePath + " at " + next[1].toLocaleTimeString());
                            const newOpts = Object.assign({}, opts, { path: filePath });
                            /// @ts-ignore
                            fableSplitter(newOpts, cachedInfo).then((info2) => {
                                if (info2.compiledPaths.size > cachedInfo.compiledPaths.size) {
                                    const newFiles = Array.from(info2.compiledPaths)
                                        .filter((x) => !cachedInfo.compiledPaths.has(x));
                                    // console.log("fable: Add " + newFiles.join())
                                    watcher.add(newFiles);
                                }
                                cachedInfo = info2;
                                onCompiled(opts, info2);
                            });
                        }
                    }
                });
        } else {
            onCompiled(opts, info, calledFromCommandLine); // Finish process only if called from command line
        }
    });
}

if (calledFromCommandLine) {
    const processArgs = process.argv.slice(2);
    switch (processArgs[0]) {
        case "-h":
        case "--help":
            console.log(getHelp());
            break;
        case "--version":
            console.log(getVersion());
            break;
        default:
            const opts = buildOptionsFromCliArgs(processArgs);
            const cfgFile = findArgValue(processArgs, "-c", "--config");
            run(opts, cfgFile);
            break;
        }
}