#!/usr/bin/env node

import { spawn } from "child_process";
import * as path from "path";
import * as readline from "readline";

const BIN_PATH = path.join(__dirname, "../bin/fable-cli/Fable.Cli.dll");

// From https://gist.github.com/LeverOne/1308368
/* tslint:disable:no-bitwise */
function uuid() {
    let b = "";
    for (let a = 0; a++ < 36;) {
        b += a * 51 & 52
            ? (a ^ 15 ? 8 ^ Math.random() * (a ^ 20 ? 16 : 4) : 4).toString(16)
            : "-";
    }
    return b;
}
/* tslint:enable:no-bitwise */

function getVersion(): string  {
    try {
        return require("../package.json").version;
    } catch {
        return "";
    }
}

function processArgs(args?: {[x: string]: any}) {
    let cliArgs = [BIN_PATH, "start-stdin"];
    if (args != null) {
        if (args.path) {
            const filePath = path.resolve(args.path);
            console.log(`dotnet binary: ${filePath}`);
            cliArgs = filePath.endsWith(".dll")
                ? [filePath, "start-stdin", "--force-pkgs"]
                : ["run", "-c", "Release", "-p", filePath, "start-stdin", "--force-pkgs"];
            delete args.path;
        }
        for (const k of Object.keys(args)) {
            cliArgs.push("--" + k.replace(/[A-Z]/g, (x) => "-" + x.toLowerCase()));
            if (args[k] !== true) {
                cliArgs.push(args[k]);
            }
        }
    }
    return cliArgs;
}

export interface ICompilerProxy {
    send(data: {}): Promise<{}>;
    close(): void;
}

export default function start(cliArgs?: {}): ICompilerProxy {
    console.log(`fable-compiler ${getVersion()}`);
    const child = spawn("dotnet", processArgs(cliArgs));

    // Error handling
    child.on("error", (err) => {
        console.error("Cannot spawn dotnet", err.message);
    });
    // child.stderr.on("data", (data) => {
    //     console.error(`child proccess errored: ${data}`);
    // });
    // child.on("close", (code) => {
    //     console.log(`child process exited with code ${code}`);
    // });

    // Pending promises
    const pending: Map<string, ((x: object) => void)> = new Map();

    const linereader = readline.createInterface({
        input: child.stdout,
    });
    linereader.on("line", (data: string) => {
        const pattern = /^JSON:([\w-]+):/.exec(data);
        if (pattern != null) {
            const id = pattern[1];
            const resolve = pending.get(id);
            if (resolve != null) {
                pending.delete(id);
                resolve(JSON.parse(data.substr(pattern[0].length)));
            }
        } else { // LOG
            console.log(data);
        }
    });

    return {
        send(data: {}) {
            return new Promise((resolve) => {
                const id = uuid();
                pending.set(id, resolve);
                child.stdin.write(`${id}:${JSON.stringify(data)}\n`);
            });
        },
        close() {
            child.stdin.write("exit\n");
        },
    };
}
