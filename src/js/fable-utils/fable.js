#!/usr/bin/env node

// This file will become an executable in UNIX systems,
// be sure not to use Windows line endings here

function getVersion() {
    return require("./package.json").version;
}

function getHelp() {
    return `Fable (F# to JS compiler) Node CLI tools ${getVersion()}
Usage: ./node_modules/.bin/fable [command] [arguments]

Commands:
  -h|--help           Show help
  --version           Print version
  clear-cache         Clear Fable cache
`
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
        case "clear-cache":
            require("./cache").clearCache();
            console.log("Cache deleted successfully");
            break;
        default:
            console.log("Unknown command: " + command);
            console.log(getHelp());
            break;
    }
}