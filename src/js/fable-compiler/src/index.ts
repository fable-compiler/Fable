#!/usr/bin/env node

import { spawn } from "child_process";
import * as path from "path";
import * as readline from "readline";
import * as uuid from "uuid/v4";

const BIN_PATH = path.join(__dirname, "../bin/fable-compiler/Fable.Compiler.dll");

function parseJson(json) {
    try {
        return JSON.parse(json);
    } catch {
        return json;
    }
}

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
    const child = spawn("dotnet", processArgs(cliArgs));
    console.log(`fable-compiler ${getVersion()}`);

    // Error handling
    child.on("error", (err) => {
        console.error(err);
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
        const pattern = /^JSON:([\w-]+):(.*)$/.exec(data);
        if (pattern != null) {
            const id = pattern[1];
            const resolve = pending.get(id);
            if (resolve != null) {
                const response = parseJson(pattern[2]);
                resolve(response);
                pending.delete(id);
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
