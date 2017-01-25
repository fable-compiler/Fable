import * as net from "net";
import * as path from "path";
import * as child_process from "child_process";
import * as chokidar from 'chokidar';
import * as fableLib from "./lib";
import { FableOptions, Continuation, BuildResult } from "./types";
import { clearBundleCache } from "./bundle";

function tooClose(opts: FableOptions, filename: string, prev: [string, Date]) {
    const d = new Date();

    return opts.watching !== "BUILDING" &&
        prev != null &&
        filename == prev[0] &&
        new Date().getTime() - prev[1].getTime() < 2000;
}

/** Watches for file changes. Requires chokidar */
export function watch(opts: FableOptions, buildResult: BuildResult, fableProc: child_process.ChildProcess, parallelProc: child_process.ChildProcess, continuation: Continuation) {
    if (opts.watching) {
        if (buildResult === "NEEDS_FULL_REBUILD") {
            fableLib.stdoutLog("Triggering full rebuild...");
            fableProc.stdin.write("[SIGFAIL]\n");
        }
        else {
            // Reset opts.watching
            opts.watching = "WATCHING";
        }
        return;
    }

    let next: [string, Date] = null, prev = null;
    (fableProc.stdin as any).setEncoding("utf-8");

    let dirs = null;
    if (typeof opts.watch === "string") {
        dirs = [fableLib.pathJoin(opts.workingDir, opts.watch)];
    }
    else if (Array.isArray(opts.watch) && opts.watch.length > 0) {
        dirs = opts.watch.map(dir => fableLib.pathJoin(opts.workingDir, dir));
    }
    else {
        dirs = opts.projFile.map(dir => path.dirname(fableLib.pathJoin(opts.workingDir, dir)));
    }

    fableLib.stdoutLog("Watching " + dirs.join('\n\t'));
    fableLib.stdoutLog("Press Enter to terminate process.");
    opts.watching = "WATCHING";

    var matchesGlobs = function (filePath) {
        var filename = fableLib.pathJoin(path.dirname(filePath), "*" + path.extname(filePath).toLowerCase());
        return dirs.indexOf(filename) > -1;
    }

    var ready = false;
    var watcher = chokidar
        .watch(dirs, { ignored: /node_modules/, persistent: true })
        .on("ready", function() { ready = true; })
        .on("all", function(ev: string, filePath: string) {
            if (ready) {
                if (fableLib.isFSharpFile(filePath) || matchesGlobs(filePath)) {
                    prev = next;
                    next = [filePath, new Date()];
                    if (!tooClose(opts, filePath, prev)) {
                        fableLib.stdoutLog(ev + ": " + filePath + " at " + next[1].toLocaleTimeString());
                        opts.watching = "BUILDING";
                        fableProc.stdin.write(filePath + "\n");
                    }
                }
            }
        });

    process.stdin.on('data', function(buf: Buffer) {
        const data = buf.toString();
        if (data.length > 0 && data[data.length - 1] == '\n') {
            if (parallelProc) {
                parallelProc.kill();
            }
            fableProc.stdin.write("[SIGTERM]\n");
            watcher.close();
            clearBundleCache(); // Clean bundle cache just in case
            fableLib.stdoutLog("Process terminated.");
            fableLib.finish(0, continuation);
        }
    });
}
