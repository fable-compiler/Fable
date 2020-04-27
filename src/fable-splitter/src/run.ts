import { ChildProcess, spawn } from "child_process";

let childCache: ChildProcess|null = null;

export default function runScript(path: string, args: string[], debug = false): Promise<number> {
    if (childCache != null) {
        childCache.kill();
    }
    const defaultArgs = debug ? ['--inspect', path] : [path];
    const child = spawn("node", defaultArgs.concat(args), {
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
