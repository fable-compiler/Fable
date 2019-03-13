const execSync = require("child_process").execSync;
const fs = require("fs");
const path = require("path");

function resolve(p) {
    return path.join(__dirname, ".paket", p);
}

if(!(fs.existsSync(resolve("paket.exe")) || fs.existsSync(resolve("paket")))) {
    execSync("dotnet tool install --tool-path \".paket\" Paket --add-source https://api.nuget.org/v3/index.json  --framework netcoreapp2.1");
    console.log("Paket installed");
}
else {
    console.log("Paket already present");
}

execSync("paket restore");
execSync("paket generate-load-scripts -f netstandard2.0 -t fsx");

