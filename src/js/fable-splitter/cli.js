#!/usr/bin/env node

/// @ts-check

var path = require("path");
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
                restArgs = process.argv.splice(2);
            }
            else {
                entry = path.resolve(command);
                restArgs = process.argv.splice(3);
            }
            var opts = {}, cfgFile = findArgValue(restArgs, ["-c", "--config"])
            if (cfgFile) {
                opts = require(path.resolve(cfgFile));
            }
            objectAssign(opts, {
                entry: entry,
                outDir: findArgValue(restArgs, ["-o", "--outDir"], path.resolve),
                watch: findFlag(restArgs, ["-w", "--watch"])
            });
            /// @ts-ignore
            fableSplitter(opts)
                .then(code => process.exit(code));
            break;
    }
}