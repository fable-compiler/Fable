#!/usr/bin/env node

import * as fs from "fs";
import * as path from "path";
import fableSplitter, { CompilationInfo } from "./index";
import runScript from "./run";

const chokidarLazy = requireLazy("chokidar");

function requireLazy(module) {
    let cache = null;
    return () => cache == null ? cache = require(module) : cache;
}

function getMainScriptPath(options, info) {

    function getFromFsproj(projectInfo) {
        const lastFile = projectInfo.projectFiles[projectInfo.projectFiles.length - 1];
        return projectInfo.mapInOutPaths.get(lastFile);
    }

    function getFromScript(scriptInfo) {
        return scriptInfo.mapInOutPaths.get(scriptInfo.entry);
    }

    const mainScriptName =
        info.projectFiles.length > 0 ? getFromFsproj(info) : getFromScript(info);

    const mainScript = path.join(options.outDir, mainScriptName);
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
  --run             [FLAG] Run script with node after compilation
                    Arguments after --run will be passed to the script

Examples:
  fable-splitter src/App.fsproj -o dist/
  fable-splitter src/ -w --run
`;
}

function findFlag(arr: string[], arg): true|null {
    return findFlagIndex(arr, arg) > -1 ? true : null;
}

// TODO: Stop after --run?
function findFlagIndex(arr: string[], arg): number {
    const args = Array.isArray(arg) ? arg : [arg];
    for (let i = 0; i < arr.length; i++) {
        if (args.indexOf(arr[i]) >= 0) {
            return i;
        }
    }
    return -1;
}

// TODO: Stop after --run?
function findArgValue(arr: string[], arg, f?): string|null {
    const args = Array.isArray(arg) ? arg : [arg];
    for (let i = 0; i < arr.length; i++) {
        if (args.indexOf(arr[i]) >= 0) {
            return typeof f === "function" ? f(arr[i + 1]) : arr[i + 1];
        }
    }
    return null;
}

function tooClose(filename: string, prev: [string, Date]|null): boolean {
    return prev != null &&
        filename === prev[0] &&
        new Date().getTime() - prev[1].getTime() < 2000;
}

/** Like Object.assign but checks first if properties in the source are null */
function objectAssignNonNull(target, source) {
    for (const k in source) {
        if (source[k] != null) {
            target[k] = source[k];
        }
    }
    return target;
}

function concatIfNotExists(ar, item): any[] {
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

function readConfig(filePath: string, force?: boolean) {
    filePath = path.resolve(filePath);
    if (fs.existsSync(filePath)) {
        return require(filePath);
    } else if (force) {
        throw new Error("Cannot read config file: " + filePath);
    } else {
        return null;
    }
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

function mustRun(args: string[]): [boolean, string[]] {
    const i = findFlagIndex(args, "--run");
    return i >= 0 ? [true, args.slice(i + 1)] : [false, []];
}

function onCompiled(args: string[], opts: any, info: CompilationInfo, mustFinish = false) {
    const hasError = Array.isArray(info.logs.error) && info.logs.error.length > 0;
    if (!hasError) {
        if (typeof opts.onCompiled === "function") {
            opts.onCompiled();
        }
        const [isRun, runArgs] = mustRun(args);
        if (isRun) {
            const job = runScript(getMainScriptPath(opts, info), runArgs);
            if (mustFinish) {
                job.then((code) => { process.exit(code); });
                return;
            }
        }
    }
    if (mustFinish) {
        process.exit(hasError ? 1 : 0);
    }

}

function run(entry, args) {
    const cfgFile = findArgValue(args, ["-c", "--config"]);

    const opts = cfgFile
        ? readConfig(cfgFile, true)
        : (readConfig("fable-splitter.config.js") || readConfig("splitter.config.js") || {});

    objectAssignNonNull(opts, {
        entry,
        outDir: findArgValue(args, ["-o", "--outDir"], path.resolve),
        allFiles: findFlag(args, "--allFiles"),
    });

    opts.entry = fixEntryPath(opts.entry);
    opts.outDir = opts.outDir || path.join(path.dirname(opts.entry), "bin");

    if (findFlag(args, ["-d", "--debug"])) {
        const fableOpts = opts.fable || {};
        fableOpts.define = concatIfNotExists(fableOpts.define, "DEBUG");
        opts.fable = fableOpts;
    }

    // If we're passing --run assume we're compiling for Node
    // TODO: In the future, we may want to use ES2015 modules and .mjs files
    if (findFlag(args, ["--commonjs", "--run"])) {
        const babelOpts = opts.babel || {};
        babelOpts.plugins = concatIfNotExists(babelOpts.define, "@babel/plugin-transform-modules-commonjs");
        opts.babel = babelOpts;
    }

    /// @ts-ignore
    console.log(`fable-splitter ${getVersion()}`);
    fableSplitter(opts).then((info) => {
        if (findFlag(args, ["-w", "--watch"])) {
            onCompiled(args, opts, info);
            let cachedInfo = info;
            let ready = false;
            let next: [string, Date]|null = null;
            let prev: [string, Date]|null = null;
            const watcher = chokidarLazy()
                .watch(Array.from(info.compiledPaths), {
                    ignored: /node_modules/,
                    persistent: true,
                })
                .on("ready", () => {
                    console.log("fable: Watching...");
                    ready = true;
                })
                .on("all", (ev, filePath) => {
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
                                onCompiled(args, opts, info2);
                            });
                        }
                    }
                });
        } else {
            onCompiled(args, opts, info, true);
        }
    });
}

const processArgs = process.argv.slice(2);
const processCmd = processArgs[0];

switch (processCmd) {
    case "-h":
    case "--help":
        console.log(getHelp());
        break;
    case "--version":
        console.log(getVersion());
        break;
    default:
        let entry: string|null = null;
        let restArgs = processArgs;
        if (processCmd && !processCmd.startsWith("-")) {
            entry = path.resolve(processCmd);
            restArgs = processArgs.slice(1);
        }
        run(entry, restArgs);
        break;
    }
