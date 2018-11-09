#!/usr/bin/env node

/// @ts-check

const fs = require("fs");
const path = require("path");
const fableSplitter = require("./index").default;
const chokidarLazy = requireLazy("chokidar");
const nodemonLazy = requireLazy("nodemon");

function requireLazy(module) {
    var cache = null;
    return function () {
        if (cache === null) {
            cache = require(module);
        }
        return cache;
    }
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
        ext: 'js'
      });

    nodemon
        // .on('start', function () { console.log('App has started'); })
        // .on('restart', function (files) { console.log('App restarted due to: ', files); })
        .on('quit', function () {
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
Usage: ./node_modules/.bin/fable-splitter [command] [arguments]

Commands:
  -h|--help         Show help
  --version         Print version
  [file path]       Compile an F# project or script to JS

Arguments:
  -o|--outDir       Output directory
  -c|--config       Config file
  -w|--watch        [FLAG] Watch mode
  --run             [FLAG] Run script with node after compilation
`
}

function findFlag(arr, arg) {
    var args = Array.isArray(arg) ? arg : [arg];
    for (var i = 0; i < arr.length; i++) {
        if (args.indexOf(arr[i]) >= 0) {
            return true;
        }
    }
    return null;
}

function findArgValue(arr, arg, f) {
    var args = Array.isArray(arg) ? arg : [arg];
    for (var i = 0; i < arr.length; i++) {
        if (args.indexOf(arr[i]) >= 0) {
            return typeof f === "function" ? f(arr[i + 1]) : arr[i + 1];
        }
    }
    return null;
}

function tooClose(filename, prev /* [string, Date] */) {
    return prev != null &&
        filename == prev[0] &&
        new Date().getTime() - prev[1].getTime() < 2000;
}

/** Like Object.assign but checks first if properties in the source are null */
function objectAssignNonNull(target, source) {
    for (var k in source) {
        if (source[k] != null) {
            target[k] = source[k];
        }
    }
    return target;
}

function readConfig(filePath, force) {
    filePath = path.resolve(filePath);
    if (fs.existsSync(filePath)) {
        return require(filePath);
    } else if (force) {
        throw new Error("Cannot read config file: " + filePath);
    } else {
        return null;
    }
}

function run(entry, args) {
    var cfgFile = findArgValue(args, ["-c", "--config"]);
    var opts = cfgFile
        ? readConfig(cfgFile, true)
        : (readConfig("fable-splitter.config.js") || readConfig("splitter.config.js") || {});
    objectAssignNonNull(opts, {
        entry: entry,
        outDir: findArgValue(args, ["-o", "--outDir"], path.resolve),
        allFiles: findFlag(args, "--allFiles")
    });

    /// @ts-ignore
    fableSplitter(opts).then(info => {
        if (findFlag(args, ["-w", "--watch"])) {
            if (findFlag(args, "--run")) {
                runScriptWithWatch(getMainScriptPath(opts, info));
            }
            var cachedInfo = info;
            var ready = false, next = null, prev = null;
            var watcher = chokidarLazy()
                .watch(Array.from(info.compiledPaths), {
                    ignored: /node_modules/,
                    persistent: true
                })
                .on("ready", function() {
                    console.log("fable: Watching...");
                    ready = true;
                })
                .on("all", function(ev, filePath) {
                    if (ready && ev === "change") {
                        prev = next;
                        next = [filePath, new Date()];
                        if (!tooClose(filePath, prev)) {
                            // console.log(ev + ": " + filePath + " at " + next[1].toLocaleTimeString());
                            var newOpts = Object.assign({}, opts, { path: filePath });
                            /// @ts-ignore
                            fableSplitter(newOpts, cachedInfo).then(info => {
                                if (info.compiledPaths.size > cachedInfo.compiledPaths.size) {
                                    var newFiles = Array.from(info.compiledPaths)
                                        .filter(x => !cachedInfo.compiledPaths.has(x));
                                    // console.log("fable: Add " + newFiles.join())
                                    watcher.add(newFiles);
                                }
                                cachedInfo = info;
                            })
                        }
                    }
                });
        }
        else {
            var hasError = Array.isArray(info.logs.error) && info.logs.error.length > 0;
            if (!hasError && findFlag(args, "--run")) {
                runScriptOnce(getMainScriptPath(opts, info));
            } else {
                process.exit(hasError ? 1 : 0);
            }
        }
    });
}

var args = process.argv.slice(2);
var command = args[0];

switch (command) {
    case "-h":
    case "--help":
        console.log(getHelp());
        break;
    case "--version":
        console.log(getVersion());
        break;
    default:
        var entry = null, restArgs = args;
        if (command && !command.startsWith('-')) {
            entry = path.resolve(command);
            restArgs = args.slice(1);
        }
        run(entry, restArgs);
        break;
    }
