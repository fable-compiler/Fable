import { ChildProcess, spawn } from "child_process";
import { RunOptions } from "./index";

let childCache: ChildProcess|null = null;

export default function runScript(path: string, opts: RunOptions): Promise<number> {
    const hostAndPort = opts.host || opts.port ? `=${opts.host ?? "127.0.0.1"}:${opts.port ?? "9229"}` : "";
    const args = (opts.inspect
        ? [`--inspect${opts.break ? "-brk" : ""}${hostAndPort}`, path]
        : [path]).concat(opts.args)

    if (childCache != null) {
        childCache.kill();
    }
    console.log("node " + args.join(" "));
    const child = spawn("node", args, {
        stdio: "inherit",
    });
    childCache = child;

    return new Promise((resolve, reject) => {
        child.on("error", (err) => {
            reject(err);
        });
        child.on("close", (code) => {
            resolve(code);
        });
    });
}
