#!/usr/bin/env node

var fableMain = require("./build");

if (require.main === module) {
    fableMain.compile();
} else {
    var fableLib = require("./lib");
    fableLib.compile = function(opts: any) {
        opts = typeof opts === "string" ? {projFile: opts} : opts;
        return fableMain.compile(opts || {});
    }
    module.exports = fableLib;
}
