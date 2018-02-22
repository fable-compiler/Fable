#!/usr/bin/env node

/// @ts-check

var path = require("path");
var chokidar = require("chokidar");
var fableSplitter = require("./index").default;

function getVersion() {
    /// @ts-ignore
    return require("./package.json").version;
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
`
}

function findFlag(arr, arg) {
    var args = Array.isArray(arg) ? arg : [arg];
    for (var i = 0; i < arr.length; i++) {
        if (args.indexOf(arr[i]) >= 0) {
            return true;
        }
    }
    return false;
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

// Like Object.assign but it checks first
// if properties in the source are null
function objectAssign(target, source) {
    for (var k in source) {
        if (source[k] != null) {
            target[k] = source[k];
        }
    }
    return target;
}

var command = process.argv[2];
if (command == null) {
    console.log(getHelp());
}
else {
    switch (command) {
        case "-h":
        case "--help":
            console.log(getHelp());
            break;
        case "--version":
            console.log(getVersion());
            break;
        default:
            var entry = null, restArgs = null;
            if (command.startsWith('-')) {
                restArgs = process.argv.slice(2);
            }
            else {
                entry = path.resolve(command);
                restArgs = process.argv.slice(3);
            }
            var opts = {}, cfgFile = findArgValue(restArgs, ["-c", "--config"])
            if (cfgFile) {
                opts = require(path.resolve(cfgFile));
            }
            objectAssign(opts, {
                entry: entry,
                outDir: findArgValue(restArgs, ["-o", "--outDir"], path.resolve)
            });
            /// @ts-ignore
            fableSplitter(opts).then(info => {
                if (findFlag(restArgs, ["-w", "--watch"])) {
                    var cachedInfo = info;
                    var ready = false, next = null, prev = null;
                    var watcher = chokidar
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
                        process.exit(hasError ? 1 : 0);
                    }
            });
            break;
    }
}