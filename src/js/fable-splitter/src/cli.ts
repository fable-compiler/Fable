#!/usr/bin/env node

import * as fs from "fs";
import * as path from "path";
import fableSplitter from "./index";

const chokidarLazy = requireLazy("chokidar");
const nodemonLazy = requireLazy("nodemon");

function requireLazy(module) {
    let cache = null;
    return () => cache == null ? cache = require(module) : cache;
}

function getMainScriptPath(options, info) {
    const lastFile = info.projectFiles[info.projectFiles.length - 1];
    const mainScript = path.join(options.outDir, info.mapInOutPaths.get(lastFile));
    return mainScript.endsWith(".js") ? mainScript : mainScript + ".js";
}

function runScriptOnce(scriptPath) {
    require(scriptPath);
}

function runScriptWithWatch(scriptPath) {
    const nodemon = nodemonLazy();
    nodemon({
        script: scriptPath,
        ext: "js",
      });

    nodemon
        // .on('start', function () { console.log('App has started'); })
        // .on('restart', function (files) { console.log('App restarted due to: ', files); })
        .on("quit", () => {
            // console.log('App has quit');
            process.exit();
        });
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
  [file path]       Compile an F# project or script to JS

Arguments:
  -o|--outDir       Output directory
  -c|--config       Config file
  -w|--watch        [FLAG] Watch mode
  -d|--debug        [FLAG] Define DEBUG constant
  --run             [FLAG] Run script with node after compilation

Example: fable-splitter src/App.fsproj -o dist/
`;
}

function findFlag(arr: string[], arg): boolean {
    const args = Array.isArray(arg) ? arg : [arg];
    for (const item of arr) {
        if (args.indexOf(item) >= 0) {
            return true;
        }
    }
    return false;
}

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
    if (findFlag(args, ["--run"])) {
        const babelOpts = opts.babel || {};
        babelOpts.plugins = concatIfNotExists(babelOpts.define, "@babel/plugin-transform-modules-commonjs");
        opts.babel = babelOpts;
    }

    /// @ts-ignore
    console.log(`fable-splitter ${getVersion()}`);
    fableSplitter(opts).then((info) => {
        if (findFlag(args, ["-w", "--watch"])) {
            if (findFlag(args, "--run")) {
                runScriptWithWatch(getMainScriptPath(opts, info));
            }
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
                            });
                        }
                    }
                });
        } else {
            const hasError = Array.isArray(info.logs.error) && info.logs.error.length > 0;
            if (!hasError && findFlag(args, "--run")) {
                runScriptOnce(getMainScriptPath(opts, info));
            } else {
                process.exit(hasError ? 1 : 0);
            }
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
