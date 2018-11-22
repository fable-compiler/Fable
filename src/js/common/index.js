const path = require("path");
const shell = require("shelljs");
shell.config.verbose = true;

module.exports = {
    runNpmBin,
    runTargets
};

const REPO_ROOT_DIR = path.join(__dirname, "../../..");

async function runNpmBin(name, args) {
    const res = await shell.exec(`node ${path.join(REPO_ROOT_DIR, "node_modules/.bin", name)} ${args || ""}`);
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