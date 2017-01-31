#!/usr/bin/env node

var fableMain = require("./build");

if (require.main === module) {
    // Check for updates
    var updateNotifier = require('update-notifier');
    var notifier = updateNotifier({
        pkg: {
            name: "fable-compiler",
            version: require("./constants").PKG_VERSION
        },
        callback: function(err, update) {
            if (err == null && update.latest !== update.current) {
                notifier.update = update;
                notifier.notify({defer: false});
            }
        }
    });

    fableMain.compile();
} else {
    var fableLib = require("./lib");
    fableLib.compile = function(opts) {
        opts = typeof opts === "string" ? {projFile: opts} : opts;
        return fableMain.compile(opts || {});
    }
    module.exports = fableLib;
}
