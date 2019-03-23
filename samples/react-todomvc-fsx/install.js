const childProcess = require("child_process");
const fs = require("fs");
const path = require("path");
const os = require("os");

/*
    Installs the dotnet tool Paket and restore the dependencies in paket.dependencies
    Then it creates the fsx load script, located at `.paket/load/netstandard2.0/main.group.fsx`
    This script is ran via the postinstall hook, see package.json
 */

function resolve(p) {
    return path.join(__dirname, ".paket", p);
}

function exec(cmd) {
    childProcess.execSync(cmd, {stdio: 'inherit'});
}

function paketCmd(cmd) {
    const file = os.type() === "Windows_NT" ? ".paket\\paket.exe" : ".paket/paket";
    const fullCmd = `${file} ${cmd}`;
    exec(fullCmd);
}

function isPaketInstalled(){
    return (fs.existsSync(resolve("paket.exe")) || fs.existsSync(resolve("paket")));
}

if(!isPaketInstalled()) {
    exec("dotnet tool install --tool-path \".paket\" Paket --add-source https://api.nuget.org/v3/index.json  --framework netcoreapp2.1");
}
else {
    console.log("Paket already present");
}

paketCmd("restore");
paketCmd("generate-load-scripts -f netstandard2.0 -t fsx");

