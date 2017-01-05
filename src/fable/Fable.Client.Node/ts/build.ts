import * as fs from "fs";
import * as path from "path";
import * as child_process from 'child_process';

import * as fableLib from "./lib";
import * as constants from "./constants";
import { Dic, FableOptions, BabelAst, BabelFile, Continuation, BuildResult } from "./types";
import { bundle, readRollupOptions } from "./bundle";
import { watch } from "./watch";
import { readOptions } from "./options";

/** Processes a JSON received from .NET process. If it's a Babel AST it will be compiled. */
function processJson(json: string, opts: FableOptions, continuation: Continuation) {
    try {
        var babelAst;
        try {
            babelAst = JSON.parse(json);
        }
        catch (_err) {
            return null; // If stdout is not in JSON format, just ignore
        }
        if (babelAst.type == "LOG") {
            if (babelAst.message.indexOf("[WARNING]") == 0) {
                fableLib.stdoutLog(babelAst.message);
            }
            else if (opts.verbose) {
                fableLib.stdoutLog(babelAst.message);
            }
        }
        else if (babelAst.type == "ERROR") {
            throw babelAst;
        }
        else if (opts.rollup) {
            return fableLib.babelify(babelAst, opts);
        }
        else {
            fableLib.babelifyToFile(babelAst, opts);
        }
    }
    catch (err) {
        fableLib.stderrLog(err);
        if (!opts.watch) {
            fableLib.finish(1, continuation);
        }
    }
    return null;
}

/** Runs the postbuild script and starts watching if necessary */
export function postbuild(opts: FableOptions, buildResult: BuildResult, fableProc: child_process.ChildProcess, continuation: Continuation) {
    var parallelProc: child_process.ChildProcess = null;
    // The "postbuild-once" script must be run only once (well done, Captain Obvious)
    // and it musn't wait till the process is finished, as it's normally used
    // to fire up watch mode of bundlers (Webpack, Rollup...)
    if (buildResult === "SUCCESS" && opts.scripts && opts.scripts["postbuild-once"]) {
        var postbuildScript = opts.scripts["postbuild-once"];
        delete opts.scripts["postbuild-once"];
        parallelProc = fableLib.runCommandInParallel(opts.workingDir, postbuildScript);
    }

    // If present, run "postbuild" script after every build and wait till it's finished
    // to exit the process or start watch mode
    if (buildResult === "SUCCESS" && opts.scripts && opts.scripts.postbuild) {
        var continuation2 = function (exitCode: number) {
            if (!opts.watch) {
                fableLib.finish(exitCode, continuation);
            }
            else {
                watch(opts, buildResult, fableProc, parallelProc, continuation);
            }
        };
        fableLib.runCommand(opts.workingDir, opts.scripts.postbuild)
            .then(continuation2, continuation2);
    }
    else if (!opts.watch) {
        fableLib.finish(buildResult === "SUCCESS" ? 0 : 1, continuation);
    }
    else {
        watch(opts, buildResult, fableProc, parallelProc, continuation);
    }
}

/** Builds the project, requires child_process */
function build(opts: FableOptions, continuation: Continuation) {
    function wrapInQuotes(arg: any) {
        if (process.platform === "win32") {
            arg = arg.toString().trim();
            return arg.indexOf(" ") > 0 && arg[0] != '"' ? '"' + arg + '"' : arg;
        }
        else {
            return arg;
        }
    };

    var fableBin = path.resolve(__dirname, "bin/Fable.Client.Node.exe");
    if (constants.PKG_NAME as string === "fable-compiler-netcore") {
        fableBin = fableBin.replace(".exe", ".dll");
    }

    var fableCmd, fableCmdArgs = [wrapInQuotes(fableBin)]
    if (constants.PKG_NAME as string === "fable-compiler-netcore") {
        fableCmd = "dotnet";
    }
    else {
        fableCmd = process.platform === "win32" ? null : "mono";
    }

    for (var k in opts) {
        if (constants.FABLE_BIN_OPTIONS.has(k)) {
            if (k === "watch" || k === "rollup")
                fableCmdArgs.push("--" + k, String(!!opts[k])); // Cast to boolean
            else if (Array.isArray((opts as any)[k]))
                (opts as any)[k].forEach((v: any) => fableCmdArgs.push("--" + k, wrapInQuotes(v)))
            else if (typeof (opts as any)[k] === "object")
                Object.getOwnPropertyNames((opts as any)[k]).forEach(k2 =>
                    fableCmdArgs.push("--" + k, wrapInQuotes(k2 + "=" + (opts as any)[k][k2])))
            else
                fableCmdArgs.push("--" + k, wrapInQuotes((opts as any)[k]));
        }
    }

    if (process.platform === "win32") {
        if (fableCmd) { fableCmdArgs.splice(0, 0, fableCmd); }
        fableCmd = "cmd";
        fableCmdArgs = ["/S", "/C", '"' + fableCmdArgs.join(" ") + '"'];
    }

    // Call Fable.exe
    if (opts.verbose) {
        fableLib.stdoutLog("\nWORKING DIR: " + opts.workingDir) + "\n";
        fableLib.stdoutLog("PROJECT FILE" + (opts.projFile.length > 1 ? "S" : "")  + ": " + opts.projFile.join("; "));
        fableLib.stdoutLog("OUTPUT DIR: " + opts.outDir);
        fableLib.stdoutLog("\nFABLE COMMAND: " + fableCmd + " " + fableCmdArgs.join(" ") + "\n");
    }
    var fableProc = child_process.spawn(fableCmd, fableCmdArgs, {
            cwd: opts.workingDir,
            windowsVerbatimArguments: true
    } as child_process.SpawnOptions);

    // Check if dotnet runtime is installed
    // !!child_process.spawnSync("which", ["dotnet"]).stdout.toString()
    // child_process.spawnSync("dotnet", ["--info"]).error != null

    fableProc.on('exit', function(code: number) {
        // There may be pending messages, do nothing here
    });

    fableProc.stderr.on('data', function(data: Buffer) {
        fableLib.stderrLog("FABLE", data.toString().substring(0, 300) + "...");
        fableLib.finish(1, continuation);
    });

    var buffer = "", jsFiles: Dic<BabelFile> = {};
    fableProc.stdout.on("data", function(data: Buffer) {
        var txt = data.toString(), newLine = 0;
        while (newLine >= 0) {
            var newLine = txt.indexOf("\n");
            if (newLine == -1) {
                buffer += txt;
            }
            else {
                var json = buffer + txt.substring(0, newLine);
                txt = txt.substring(newLine + 1);
                buffer = "";
                var buildFinished = /^\s*\[SIG(SUCCESS|FAIL)\]\s*$/.exec(json);
                if (buildFinished) {
                    var buildSuccess = buildFinished[1] === "SUCCESS";
                    if (opts.rollup && buildSuccess) {
                        bundle(jsFiles, opts, fableProc, continuation);
                    }
                    else if (opts.inMemory) {
                        if (buildSuccess)
                            continuation.resolve(jsFiles);
                        else
                            continuation.reject("Build failed");
                    }
                    else {
                        const buildResult: BuildResult =
                            buildSuccess ? "SUCCESS" : "FAIL";
                        postbuild(opts, buildResult, fableProc, continuation);
                    }
                }
                else {
                    var res = processJson(json, opts, continuation);
                    if (Array.isArray(res))
                        res.forEach(file => jsFiles[file.fileName] = file);
                }
            }
        }
    });
}

function main(opts: FableOptions, continuation?: Continuation) {
    fableLib.stdoutLog(constants.PKG_NAME + " " + constants.PKG_VERSION + ": Start compilation...");
    try {
        opts = readOptions(opts);
        if (opts.scripts && opts.scripts.prebuild) {
            var continuation2 = function (exitCode: number) {
                if (exitCode === 0) { build(opts, continuation); }
                else { fableLib.finish(exitCode, continuation); }
            };
            fableLib.runCommand(opts.workingDir, opts.scripts.prebuild)
                .then(continuation2, continuation2);
        }
        else {
            build(opts, continuation);
        }
    }
    catch (err) {
        fableLib.stderrLog("OPTIONS", err);
        fableLib.finish(1, continuation);
    }
}

/**
 * Starts compilation, if opts is not empty assumes it's
 * running from API and returns a Promise.
*/
export function compile(opts?: FableOptions): Promise<any> | void {
    if (opts) {
        return new Promise(function (resolve, reject) {
            main(opts, { resolve: resolve, reject: reject });
        });
    }
    main(opts);
}
