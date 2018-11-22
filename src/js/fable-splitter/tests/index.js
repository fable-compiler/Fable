const shell = require("shelljs");
const ava = require("ava");

ava.test("allFiles in config works", (t) => {
    shell.cd(__dirname);
    shell.rm("-rf", "temp")
    shell.exec("node ../dist/cli allFiles -o temp -c allFiles/config.js");
    const files = shell.ls("temp").slice();
    t.true(files.includes("File1.js"));
    t.true(files.includes("File2.js"));
});
