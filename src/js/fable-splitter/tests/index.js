const shell = require("shelljs");
const assert = require('assert');

describe("CLI", function () {
    it("allFiles in config works", function () {
        shell.cd(__dirname);
        shell.rm("-rf", "temp")
        shell.exec("node ../dist/cli allFiles -o temp -c allFiles/config.js");
        const files = shell.ls("temp");
        assert.equal(files.includes("File1.js"), true);
        assert.equal(files.includes("File2.js"), true);
    })
})

