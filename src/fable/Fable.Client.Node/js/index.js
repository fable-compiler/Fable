#!/usr/bin/env node

var fableMain = require("./fable");

if (require.main === module) {
    fableMain.run();
} else {
    module.exports = function(opts) {
        if (typeof opts !== "object")
            throw "You must pass an object with Fable options";
        return fableMain.run(opts);
    }
}
