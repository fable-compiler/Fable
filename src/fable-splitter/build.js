const path = require("path");
const shell = require("shelljs");

shell.config.verbose = true;
const REPO_ROOT_DIR = path.join(__dirname, "../..");

runTargets({
    async test() {
        await runNpmBin("ava", "tests/index.js");
    },
    async build() {
        // await runNpmBin("tslint", "--project .");
        await runNpmBin("tsc");
    }
});

// UTILS ---------------------------

async function runNpmBin(name, args) {
    const isWin = process.platform === "win32";
    const res = await shell.exec(`${path.join(REPO_ROOT_DIR, "node_modules/.bin", name + (isWin ? ".cmd" : ""))} ${args || ""}`);
    if (res.code !== 0) {
        throw new Error(`${name} failed with code ${res.code}`);
    }
}

function runTargets(targets) {
    const args = process.argv.slice(2);
    const [target, restArgs] =
        typeof args[0] === "string" && !args[0].startsWith("--")
        ? [args[0].toLowerCase(), args.splice(1)]
        : ["build", args];
    const res = targets[target].apply(targets, restArgs);
    if (res instanceof Promise) {
        res.then(() => {}, er => {
            console.error(er);
            process.exit(1);
        })
    }
}